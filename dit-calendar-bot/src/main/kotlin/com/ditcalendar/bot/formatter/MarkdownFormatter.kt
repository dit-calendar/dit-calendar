package com.ditcalendar.bot.formatter

import com.ditcalendar.bot.config.bot_name
import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.data.*


private val config by config()

private val botName = config[bot_name]

fun TelegramTaskAssignment.toMarkdown(): String =
        when (this.task) {
            is TaskForAssignment ->
                """
                    _Task_: ${task.description} [assign me](https://t.me/$botName?start=assign_${task.taskId})
                    _Datum_: ${task.startTime}
                """.trimIndent()
            is TaskForUnassignment ->
                """
                    _Task_: ${task.description}
                    _Datum_: ${task.startTime}
                """.trimIndent()
            is TaskAfterUnassignment ->
                """
                    *erfolgreich ausgetragen von*:
                    _Task_: ${task.description}
                    _Datum_: ${task.startTime}
                """.trimIndent()
        }

fun TelegramTaskAssignments.toMarkdown(): String = joinToString(separator = System.lineSeparator()) { it.toMarkdown() }

fun DitCalendar.toMarkdown(): String =
        """
            *$description*
            *Datum*: $startDate
            
        """.trimIndent() + telegramTaskAssignments.toMarkdown()