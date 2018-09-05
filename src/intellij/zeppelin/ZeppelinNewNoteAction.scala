package intellij.zeppelin

import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.editor.Editor

class ZeppelinNewNoteAction extends ZeppelinAction {

  override def actionPerformed(anActionEvent: AnActionEvent): Unit = {

    val selectedText = currentSelectedText(currentEditor(anActionEvent))
    val name = if (selectedText.isEmpty) "IntelliJ Notebook" else selectedText
    val api = zeppelin(anActionEvent)
    api.createNotebook(name).map { notebook =>
        show(s"Created new Zeppelin notebook '$name': ${notebook.id}")

        runWriteAction(anActionEvent){ _ =>
          insertBefore(currentEditor(anActionEvent), notebook, api.url)
        }

    } recover { case t: Throwable => show(t.toString) }
  }
}


