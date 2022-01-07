package fitnesse.idea.run

import javax.swing.Icon

import com.intellij.execution.JavaRunConfigurationExtensionManager
import com.intellij.execution.actions.ConfigurationContext
import com.intellij.execution.configurations._
import com.intellij.execution.junit.JavaRunConfigurationProducerBase
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.{ProjectFileIndex, ProjectRootManager}
import com.intellij.openapi.util.Ref
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile}
import com.intellij.psi.{PsiDirectory, PsiElement, PsiFile}
import fitnesse.idea.etc.FitnesseBundle
import fitnesse.idea.filetype.FitnesseFileType

class FitNesseTestRunConfigurationProducer extends JavaRunConfigurationProducerBase[FitnesseRunConfiguration](FitnesseRunConfigurationType.INSTANCE) {

  override def setupConfigurationFromContext(configuration: FitnesseRunConfiguration, context: ConfigurationContext, sourceElement: Ref[PsiElement]): Boolean =
    wikiPageInfo(configuration, context) match {
      case None => false
      case Some((wikiPageFile, fitnesseRoot)) =>
        val wikiPageName = makeWikiPageName(fitnesseRoot, wikiPageFile)

        configuration.fitnesseRoot = fitnesseRoot.getName
        configuration.setWorkingDirectory(fitnesseRoot.getParent.getCanonicalPath)
        configuration.wikiPageName = wikiPageName

        configuration.setName(wikiPageName)

        setupConfigurationModule(context, configuration)
        JavaRunConfigurationExtensionManager.getInstance().extendCreatedConfiguration(configuration, context.getLocation)
        true
    }

  override def isConfigurationFromContext(configuration: FitnesseRunConfiguration, context: ConfigurationContext): Boolean =
    wikiPageInfo(configuration, context) match {
      case None => false
      case Some((wikiPageFile, fitnesseRoot)) =>
        val wikiPageName = makeWikiPageName(fitnesseRoot, wikiPageFile)
        configuration.fitnesseRoot == fitnesseRoot.getName &&
          configuration.getWorkingDirectory == fitnesseRoot.getParent.getCanonicalPath &&
          configuration.wikiPageName == wikiPageName
    }

  def wikiPageInfo(configuration: FitnesseRunConfiguration, context: ConfigurationContext): Option[(VirtualFile, VirtualFile)] =
    findWikiPageFile(context) match {
      case None => None
      case Some(wikiPageFile) => findFitnesseRoot(configuration, ProjectRootManager.getInstance(configuration.getProject).getFileIndex, wikiPageFile) match {
        case None => None
        case Some(fitnesseRoot) => Some((wikiPageFile, fitnesseRoot))
      }
    }

  def findWikiPageFile(context: ConfigurationContext): Option[VirtualFile] =
    Option(context.getPsiLocation) match {
      case Some(directory: PsiDirectory) =>
        if (Option(directory.getChildren).getOrElse(Array.empty).exists {
          case file: PsiFile => file.getFileType == FitnesseFileType.INSTANCE
          case _ => false
        }) {
          Some(directory.getVirtualFile)
        } else {
          None
        }
      case Some(elem: PsiElement) if elem.getContainingFile.getFileType == FitnesseFileType.INSTANCE =>
        val file = elem.getContainingFile
        file.getName match {
          case FitnesseFileType.CONTENT_TXT_NAME => Some(file.getParent.getVirtualFile)
          case name if FitnesseFileType.WIKI_FILE_EXTENSION_MATCHER.accept(name) => Some(file.getVirtualFile)
          case _ => None
        }
      case _ => None
    }

  def findFitnesseRoot(configuration: FitnesseRunConfiguration, fileIndex: ProjectFileIndex, page: VirtualFile): Option[VirtualFile] = {
    if (page == null || !fileIndex.isInContent(page)) {
      None
    } else if (page.getName == configuration.getFitnesseRoot) {
      Some(page)
    } else {
      findFitnesseRoot(configuration, fileIndex, page.getParent)
    }
  }

  def makeWikiPageName(fitnesseRoot: VirtualFile, wikiPageFile: VirtualFile) = {
    val pagePath = VfsUtilCore.getRelativePath(wikiPageFile, fitnesseRoot, '.')
    if (FitnesseFileType.WIKI_FILE_EXTENSION_MATCHER.accept(wikiPageFile.getName)) {
      pagePath.substring(0, pagePath.length - FitnesseFileType.WIKI_FILE_EXTENSION.length - 1)
    } else {
      pagePath
    }
  }
}

class FitnesseRunConfigurationType extends ConfigurationType {
  val fitnesseRunConfigurationFactory: ConfigurationFactory = new ConfigurationFactory(this) {
    val ID = "FitnesseRunConfigurationFactory"

    override def getId = ID

    override def getIcon: Icon = FitnesseFileType.FILE_ICON

    override def createTemplateConfiguration(project: Project) = new FitnesseRunConfiguration(getDisplayName(), project, this)
  }

  override def getDisplayName = FitnesseBundle.message("configurationtype.displayname")

  override def getConfigurationTypeDescription = FitnesseBundle.message("configurationtype.description")

  override def getIcon = FitnesseFileType.FILE_ICON

  override def getId = FitnesseRunConfigurationType.ID

  override def getConfigurationFactories: Array[ConfigurationFactory] = Array(fitnesseRunConfigurationFactory)
}

object FitnesseRunConfigurationType {
  val ID = "FitnesseRunConfigurationType"

  def INSTANCE = try {
    ConfigurationTypeUtil.findConfigurationType(classOf[FitnesseRunConfigurationType])
  } catch {
    case _: java.lang.IllegalArgumentException => new FitnesseRunConfigurationType()
  }
}
