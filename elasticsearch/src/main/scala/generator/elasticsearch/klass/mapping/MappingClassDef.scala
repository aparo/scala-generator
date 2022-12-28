/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.klass.mapping

import generator.elasticsearch.klass.{BaseClassDefTrait, ClassDef}

/** Created by alberto on 10/04/2017.
  */
case class MappingClassDef(classDef: ClassDef) extends BaseClassDefTrait {
  override def extraCaseClassScala: String = this.renderToJSON

  override def extraObjectScala: String = ""
}
