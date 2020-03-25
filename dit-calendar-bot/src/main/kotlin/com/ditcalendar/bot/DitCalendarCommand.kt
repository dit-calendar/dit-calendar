package com.ditcalendar.bot

import com.ditcalendar.bot.data.TelegramLink
import com.ditcalendar.bot.endpoint.AuthEndpoint
import com.ditcalendar.bot.endpoint.CalendarEndpoint
import com.ditcalendar.bot.endpoint.TaskEndpoint
import com.ditcalendar.bot.error.DitBotError
import com.ditcalendar.bot.error.InvalidRequest
import com.elbekD.bot.types.Message
import com.elbekD.bot.types.User
import com.github.kittinunf.fuel.core.FuelError
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.flatMap
import com.github.kittinunf.result.map

class DitCalendarCommand {

    private val calendarEndpoint = CalendarEndpoint()
    private val taskEndpoint = TaskEndpoint()
    private val authEndpoint = AuthEndpoint()

    fun parseRequest(msg: Message, opts: String?): String {

        val result = if (opts == null || !opts.startsWith("assign")) {
            val calendarId: Long = 1
            getCalendarAndTask(calendarId)
        } else {
            val msgUser = msg.from
            val taskId: Long? = opts.substringAfter("_").toLongOrNull()

            if (msgUser != null && taskId != null)
                assignUserToTask(taskId, msg.chat.id, msgUser)
            else
                Result.error(InvalidRequest())
        }

        return parseResponse(result)

    }

    private fun parseResponse(result: Result<String, Exception>): String =
            when (result) {
                is Result.Success ->
                    result.value
                is Result.Failure -> {
                    result.error.printStackTrace()
                    when (val error = result.error) {
                        is FuelError -> {
                            when (error.response.statusCode) {
                                403 -> "Bot fehlen notwendige Zugriffsrechte"
                                404 -> "Kalendar oder Task nicht gefunden"
                                503 -> "Server nicht erreichbar, versuchs nochmal"
                                else -> "unbekannter Fehler"
                            }
                        }
                        is DitBotError -> {
                            when (error) {
                                is InvalidRequest -> "fehlerhafte Anfrage"
                            }
                        }
                        else -> "unbekannter Fehler"
                    }
                }
            }

    private fun getCalendarAndTask(calendarId: Long): Result<String, Exception> {
        val calendarResult = calendarEndpoint.readCalendar(calendarId)
        val tokenResul = authEndpoint.getToken()
        val tasksResulst = tokenResul.flatMap { taskEndpoint.readTasks(calendarId, it) }

        return calendarResult.flatMap { calendar ->
            tasksResulst.map {
                calendar.apply { tasks = it }
            }
        }.map { it.toStringInMarkdown() + System.lineSeparator() }
    }

    private fun assignUserToTask(taskId: Long, chatId: Long, user: User): Result<String, Exception> {
        val telegramLink = TelegramLink(chatId, user.id, user.username)
        val taskAfterAssignment = authEndpoint.getToken()
                .flatMap { taskEndpoint.assignUserToTask(taskId, telegramLink, it) }

        return taskAfterAssignment.map {
            """
                *erfolgreich hinzugefügt zu:*
                ${it.toStringInMarkdown()}
            """.trimIndent()
        }
    }
}