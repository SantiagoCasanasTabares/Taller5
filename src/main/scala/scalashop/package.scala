package object scalashop {
  /** El valor de cada pixel es representado con un entero de 32 bits. */
  type RGBA = Int

  /** Devuelve el componente de rojo */
  def rojo(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Devuelve el componente de verde */
  def verde(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Devuelve el componente de azul */
  def azul(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Devuelve el componente  alpha */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Usado para crear un valor RGBA a partir de sus componentes */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restringe el entero al rango especificado */
  def cercar(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** La imagen es un arreglo de dos dimensiones de valores de pixeles */
  class Img(val ancho: Int, val alto: Int, private val datos: Array[RGBA]) {
    def this(an: Int, al: Int) = this(an, al, new Array(an*al))
    def apply(x: Int, y: Int): RGBA = datos(y * ancho + x)
    def update(x: Int, y: Int, c: RGBA): Unit = datos(y * ancho + x) = c
  }

  /** Calcula el valor RGBA del pixel desenfocado correspondiente a un pixel de la imagen de entrada. */
  def desenfoqueNuclear(fte: Img, x: Int, y: Int, radio: Int): RGBA = fte.apply(x,y)
    // POR HACER: puede implementarla usando ciclos while o expresiones for


}
