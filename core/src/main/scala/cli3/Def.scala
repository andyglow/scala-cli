package cli3

import scala.collection.mutable

object Def {

  /* +----------
   * | Base Prop trait
   * | There are 4 type of props
   * | - keyed
   * |   - flags
   * |   - opts
   * | - indexed
   * |   - args
   * |   - cmds
   * +------------------
   */
  trait Prop

  trait Keyed extends Prop {
    def key: Key
    def repetitive: Boolean
    private[cli3] def occurrences: Int
  }

  final case class KeyedProps private(elements: List[Keyed] = Nil) {

    def isEmpty: Boolean = elements.isEmpty

    def nonEmpty: Boolean = !isEmpty

    def :+(x: Keyed): KeyedProps = copy(elements = elements :+ x)

    private[cli3] def occurred(x: Keyed): KeyedProps = {
      def marked(k: Key) = elements map {
        case v: Flag if v.key =:= k => v.occurred
        case v: Opt if v.key =:= k  => v.occurred
        case v                      => v
      }

      copy(elements = marked(x.key))
    }
  }

  sealed trait Indexed extends Prop

  trait IndexedProps

  object IndexedProps {

    final case object Empty extends IndexedProps
  }

  private[cli3] sealed trait PropKind

  private[cli3] object PropKind {
    private[cli3] final case object Flag extends PropKind
    private[cli3] final case object Opt extends PropKind
  }

  /* +----------
   * | Flags
   * +------------------
   */
  final case class Flag(
    key: Key,
    repetitive: Boolean = false,
    private[cli3] val occurrences: Int = 0) extends Keyed {

    def `∞`: Flag = copy(repetitive = true)

    def :+(x: Flag): KeyedProps = KeyedProps(List(this, x))

    private[cli3] def occurred: Flag = copy(occurrences = occurrences + 1)

    override def toString: String = key.toString

    override def hashCode(): Int = key.hashCode()

    override def equals(x: Any): Boolean = this.canEqual(x) && {
      x.asInstanceOf[Flag].key =:= key
    }
  }

  /* +----------
   * | Opts
   * +------------------
   */
  final case class Opt(
    key: Key,
    required: Boolean = false,
    repetitive: Boolean = false,
    valueLabel: String = "value",
    text: Option[String] = None,
    private[cli3] val occurrences: Int = 0) extends Keyed {

    def `!`: Opt = copy(required = true)

    def `?`: Opt = copy(required = false)

    def `∞`: Opt = copy(repetitive = true)

    def :+(x: Opt): KeyedProps = KeyedProps(List(this, x))

    def value(x: String): Opt = copy(valueLabel = x)

    def text(x: String): Opt = copy(text = Some(x))

    private[cli3] def occurred: Opt = copy(occurrences = occurrences + 1)

    override def toString: String = key.toString

    override def hashCode(): Int = key.hashCode()

    override def equals(x: Any): Boolean = canEqual(x) && {
      x.asInstanceOf[Opt].key =:= key
    }
  }

  /* +----------
   * | Args
   * +------------------
   */
  sealed trait Arg extends Indexed

  object Arg {

    final case class Arg1(label: String, required: Boolean = false) extends Arg {

      def `!`: Arg1 = copy(required = true)

      def :+(x: Arg1): Args.Growable = Args() :+ this.! :+ x

      def :+(x: VarArg): Args.Final = Args() :+ this.! :+ x

      override def toString: String = label
    }

    final case class VarArg(label: String, minOcc: Int = 0) extends Arg {

      override def toString: String = s"..$label"
    }

    def apply(label: String, required: Boolean = false): Arg1 = Arg1(label, required)
  }

  object VarArg {

    def apply(label: String, minOcc: Int = 0): Arg.VarArg = Arg.VarArg(label, minOcc)
  }

  sealed trait Args extends IndexedProps {

    def isEmpty: Boolean = elements.isEmpty

    def nonEmpty: Boolean = !isEmpty

    def elements: List[Arg]
  }

  object Args {

    final case class Growable private(elements: List[Arg.Arg1] = Nil) extends Args {

      def :+(x: Arg.Arg1): Growable = copy(elements = elements :+ x)

      def -(x: Arg.Arg1): Growable = copy(elements = elements.filterNot(_ == x))

      def :+(x: Arg.VarArg): Final = {
        if (x.minOcc > 0) Final(elements.map(_.!), x) else Final(elements, x)
      }
    }

    final case class Final private(ones: List[Arg.Arg1] = Nil, last: Arg.VarArg) extends Args {

      def -(x: Arg.Arg1): Final = copy(ones = ones.filterNot(_ == x))

      def elements: List[Arg] = ones :+ last
    }

    def apply(): Growable = Growable(Nil)
  }

  /* +----------
   * | Cmds
   * +------------------
   */
  case class Cmd private[cli3](
    program: String,
    title: Option[String]   = None,
    version: Option[String] = None,
    header: Option[String]  = None,
    keyedProps: KeyedProps  = KeyedProps(),
    indexedProps: IndexedProps = IndexedProps.Empty,
    footer: Option[String]  = None) extends Indexed {

    private[cli3] lazy val flags = keyedProps.elements collect { case f: Flag => f }
    private[cli3] lazy val opts  = keyedProps.elements collect { case o: Opt => o }
    private[cli3] lazy val args  = indexedProps match {
      case args: Args => args.elements
      case _          => Nil
    }
    private[cli3] lazy val cmds  = indexedProps match {
      case cmds: Cmds => cmds.elements
      case _          => Nil
    }

    private[cli3] def resolveKeyed(rawKey: String): Option[Keyed] = {
      val k = if (rawKey.length == 1) Key.Short(rawKey.head) else Key.Long(rawKey)

      val isFlag = flags.collectFirst { case x if x.key =:= k => x }
      def isOpt  = opts.collectFirst { case x if x.key =:= k => x }

      isFlag orElse isOpt
    }

    private[cli3] def resolveIndexed(rawVal: String): Option[Indexed] = {
      indexedProps match {
        // args is taken in the same order as it was defined
        case args: Args => args.elements.headOption
        // for commands we must find a matching element
        case cmds: Cmds => cmds.elements.find(_.program == rawVal)
        // for empty - ignore
        case IndexedProps.Empty => None
      }
    }

    private[cli3] def occurred(x: Prop): Either[RecognizeError, Cmd] = x match {
      case f: Keyed if !f.repetitive && f.occurrences > 0 => Left(RecognizeError.RepetitionOfNonRepetitive(f))
      case f: Keyed       => Right(copy(keyedProps = keyedProps occurred f))
      case _: Arg.VarArg  => Right(copy(indexedProps = indexedProps match {
        case args: Args.Final    => args.copy(last = args.last.copy(minOcc = 0 max (args.last.minOcc - 1)))
        case args: Args.Growable => args
        case cmds: Cmds          => cmds
      }))
      case a: Arg.Arg1    => Right(copy(indexedProps = indexedProps match {
        case args: Args.Growable => args - a
        case args: Args.Final    => args - a
        case cmds: Cmds          => cmds
      }))
      //    case a: Cmd =>this
    }

    def requiredProps: Seq[Prop] = opts.filter { x => x.required && x.occurrences <= 0 } ++ args.collect {
      case a @ Arg.Arg1(_, true) => a
      case a @ Arg.VarArg(_, m) if m > 0 => a
    }

    override def toString: String = {
      val f = if (flags.isEmpty) "" else flags.mkString("Flags(", ",", ")")
      val o = if (opts.isEmpty) "" else opts.mkString("Opts(", ",", ")")
      val a = if (args.isEmpty) "" else args.mkString("Args(", ",", ")")
      val c = if (cmds.isEmpty) "" else cmds.mkString("Cmds(", ",", ")")

      if (f.isEmpty && o.isEmpty && a.isEmpty && c.isEmpty) "Empty" else s"CLI($f,$o,$a,$c)"
    }
  }

  object Cmd {

    private implicit class MultiMapOps[K](private val m: mutable.HashMap[K, List[PropKind]]) extends AnyVal {
      def add(k: K, kind: PropKind): Unit = m.updateWith(k) {
        case None       => Some(List(kind))
        case Some(list) => Some(list :+ kind)
      }
    }

    def apply(
      program: String,
      title: Option[String]   = None,
      version: Option[String] = None,
      header: Option[String]  = None,
      keyedProps: KeyedProps = KeyedProps(),
      indexedProps: IndexedProps = IndexedProps.Empty,
      footer: Option[String] = None): String Either Cmd = {
      import Key._

      val shortOccurrences = mutable.HashMap.empty[Char, List[PropKind]]
      val longOccurrences  = mutable.HashMap.empty[String, List[PropKind]]

      keyedProps.elements.map(_.key) foreach {
        case Short(s)   => shortOccurrences.add(s, PropKind.Flag)
        case Long(l)    => longOccurrences.add(l, PropKind.Flag)
        case Full(s, l) => shortOccurrences.add(s, PropKind.Flag); longOccurrences.add(l, PropKind.Flag)
      }

      val inconsistencies = mutable.ListBuffer.empty[String]

      shortOccurrences foreach { case (s, list) =>
        if (list.size > 1) {
          val (asFlag, asOpt) = list partition { _ == PropKind.Flag }
          inconsistencies += s"'$s' can't be used for more then one definition. In fact it is used ${asFlag.size} times for Flag and ${asOpt.size} times for Opt"
        }
      }

      longOccurrences foreach { case (l, list) =>
        if (list.size > 1) {
          val (asFlag, asOpt) = list partition { _ == PropKind.Flag }
          inconsistencies += s""""$l" can't be used for more then one definition. In fact it is used ${asFlag.size} times for Flag and ${asOpt.size} times for Opt"""
        }
      }

      if (inconsistencies.nonEmpty) Left {
        s"Inconsistent CLI definition:\n${inconsistencies map { x => s"- $x" } mkString "\n" }"
      } else Right(new Cmd(program, title, version, header, keyedProps, indexedProps, footer))
    }
  }

  final case class Cmds private(elements: List[Cmd] = Nil) extends IndexedProps {
    def isEmpty: Boolean = elements.isEmpty
    def nonEmpty: Boolean = !isEmpty
    def :+(x: Cmd) : Cmds  = copy(elements = elements :+ x)
  }
}
