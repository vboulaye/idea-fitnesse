package com.gshakhn.idea.idea.fitnesse.lang.psi

import com.gshakhn.idea.idea.fitnesse.lang.manipulator.FixtureClassManipulator
import com.gshakhn.idea.idea.fitnesse.lang.reference.FixtureClassReference
import com.intellij.lang.ASTNode
import com.intellij.psi.search.{GlobalSearchScope, PsiShortNamesCache}
import com.intellij.psi.util.ClassUtil.getJVMClassName
import com.intellij.psi.{PsiClass, JavaPsiFacade, PsiElement, PsiNamedElement}
import fitnesse.testsystems.slim.tables.Disgracer.disgraceClassName

class FixtureClass(node: ASTNode) extends Cell(node) with PsiNamedElement {

  override def fixtureClassName: Option[String] = disgraceClassName(node.getText.trim) match {
    case "" => None
    case className => Some(className)
  }

  def isQualifiedName: Boolean = {
    fixtureClassName match {
      case Some(name) =>
        val dotIndex: Int = name.indexOf(".")
        dotIndex != -1 && dotIndex != name.length - 1
      case None => false
    }
  }

  def shortName: Option[String] = {
    fixtureClassName match {
      case Some(name) => name.split('.').toList.reverse match {
        case "" :: n :: _ => Some(n)
        case n :: _ => Some(n)
        case _ => Some(name)
      }
      case None => None
    }
  }

  def getReferencedClasses: Array[PsiClass] = {
    fixtureClassName match {
      case Some(className) =>
        if (isQualifiedName) {
          val classFound = JavaPsiFacade.getInstance(getProject).findClass(className, GlobalSearchScope.projectScope(getProject));
          if (classFound != null) {
            Array(classFound)
          } else {
            Array()
          }
        } else {
          PsiShortNamesCache.getInstance(getProject)
          .getClassesByName(shortName.get, GlobalSearchScope.projectScope(getProject))
        }
      case None => Array()
    }
  }

  override def getReferences = {
    getReferencedClasses.map(new FixtureClassReference(_, this))
  }

//  override def getName = fixtureClassName.get

  override def setName(s: String): PsiElement = FixtureClassManipulator.createFixtureClass(getProject, s)
}