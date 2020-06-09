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
                    *${task.formatTime()}* \- ${task.title}
                    Wer?: ${assignedUsers.toMarkdown()} [assign me](https://t.me/$botName?start=assign_${task.taskId})
                """.trimIndent()
            is TaskForUnassignment -> {
                val formattedDescription =
                        if (task.description != null)
                            System.lineSeparator() + task.description.toString()
                        else ""
                "*erfolgreich hinzugefügt:*" + System.lineSeparator() +
                        "*${formatter.format(task.startTime.time)} Uhr* \\- ${task.title}$formattedDescription" + System.lineSeparator() +
                        "Wer?: ${assignedUsers.toMarkdown()}"
            }

            is TaskAfterUnassignment ->
                """
                    *erfolgreich ausgetragen*:
                    *${formatter.format(task.startTime.time)} Uhr* \- ${task.title}
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


fun TelegramTaskAssignments.toMarkdown(): String = System.lineSeparator() +
        joinToString(separator = System.lineSeparator()) { it.toMarkdown() }

fun DitCalendar.toMarkdown(): String {
    val formattedDescription =
            if (description != null)
                System.lineSeparator() + description.toString() + System.lineSeparator()
            else ""

    return """
            *$title* am ${dateFormatter.format(startDate).replace(".", "\\.")}$formattedDescription
        """.trimIndent() + telegramTaskAssignments.toMarkdown()
}