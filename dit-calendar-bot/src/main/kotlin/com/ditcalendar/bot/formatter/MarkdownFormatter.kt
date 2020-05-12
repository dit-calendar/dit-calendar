package com.ditcalendar.bot.formatter

import com.ditcalendar.bot.config.bot_name
import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.data.*
import java.text.SimpleDateFormat


private val config by config()

private val botName = config[bot_name]

private val formatter = SimpleDateFormat("HH:mm")

private val dateFormatter = SimpleDateFormat("dd.MM")

fun TelegramTaskAssignment.toMarkdown(): String =
        when (this.task) {
            is TaskForAssignment ->
                """
                    *${task.formatTime()}* \- ${task.description}
                    Wer?: ${assignedUsers.toMarkdown()} [assign me](https://t.me/$botName?start=assign_${task.taskId})
                """.trimIndent()
            is TaskForUnassignment ->
                """
                    *erfolgreich hinzugefügt:*
                    *${formatter.format(task.startTime.time)} Uhr* \- ${task.description}
                    Wer?: ${assignedUsers.toMarkdown()}
                """.trimIndent()
            is TaskAfterUnassignment ->
                """
                    *erfolgreich ausgetragen*:
                    *${formatter.format(task.startTime.time)} Uhr* \- ${task.description}
                """.trimIndent()
        }

private fun Task.formatTime(): String {
    var timeString = formatter.format(this.startTime)
    timeString += if (this.endTime != null) " \\- " + formatter.format(this.endTime) else ""
    return timeString + " Uhr"
}

@JvmName("toMarkdownForTelegramLinks")
private fun TelegramLinks.toMarkdown(): String {
    var firstNames = this.filter { it.firstName != null }.joinToString(", ") { it.firstName!! }
    val anonymousCount = this.count { it.firstName == null }
    firstNames += if (anonymousCount != 0) " \\+$anonymousCount" else ""
    return firstNames
}


fun TelegramTaskAssignments.toMarkdown(): String = joinToString(separator = System.lineSeparator()) { it.toMarkdown() }

fun DitCalendar.toMarkdown(): String =
        """
            *$description* am ${dateFormatter.format(startDate).replace(".", "\\.")}
            
        """.trimIndent() + telegramTaskAssignments.toMarkdown()