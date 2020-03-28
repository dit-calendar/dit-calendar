package com.ditcalendar.bot

import com.ditcalendar.bot.data.TelegramLink
import com.ditcalendar.bot.endpoint.AuthEndpoint
import com.ditcalendar.bot.endpoint.CalendarEndpoint
import com.ditcalendar.bot.endpoint.TaskEndpoint
import com.ditcalendar.bot.error.DitBotError
import com.ditcalendar.bot.error.InvalidRequest
import com.ditcalendar.bot.error.UnassigmentError
import com.ditcalendar.bot.markdown.toMarkdown
import com.github.kittinunf.fuel.core.FuelError
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.flatMap
import com.github.kittinunf.result.map

class DitCalendarCommand {

    private val calendarEndpoint = CalendarEndpoint()
    private val taskEndpoint = TaskEndpoint()
    private val authEndpoint = AuthEndpoint()

    fun parseRequest(telegramLink: TelegramLink, opts: String?): String {

        val result =
                if (opts != null && opts.startsWith("assign")) {
                    val taskId: Long? = opts.substringAfter("_").toLongOrNull()
                    if (taskId != null)
                        assignUserToTask(taskId, telegramLink)
                    else
                        Result.error(InvalidRequest())
                } else if (opts != null && opts.startsWith("unassign")) {
                    val taskId: Long? = opts.substringAfter("_").toLongOrNull()
                    if (taskId != null)
                        unassignUserFromTask(taskId, telegramLink)
                    else
                        Result.error(InvalidRequest())
                } else {
                    val calendarId: Long = 1
                    getCalendarAndTask(calendarId)
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
                                is UnassigmentError -> "fehlgeschlagen"
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
        }.map { it.toMarkdown() + System.lineSeparator() }
    }

    private fun assignUserToTask(taskId: Long, telegramLink: TelegramLink): Result<String, Exception> {
        val taskAfterAssignment = authEndpoint.getToken()
                .flatMap { taskEndpoint.assignUserToTask(taskId, telegramLink, it) }

        return taskAfterAssignment.map {
            "*erfolgreich hinzugefügt zu:*" + System.lineSeparator() + it.toMarkdown()
        }
    }

    private fun unassignUserFromTask(taskId: Long, telegramLink: TelegramLink): Result<String, Exception> {
        return authEndpoint.getToken()
                .flatMap { taskEndpoint.unassignUserFromTask(taskId, telegramLink, it) }
                .fold({ Result.Success(it.toMarkdown()) }, { Result.error(UnassigmentError()) })
    }
}