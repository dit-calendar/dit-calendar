package com.ditcalendar.bot

import com.ditcalendar.bot.data.TelegramLink
import com.ditcalendar.bot.endpoint.AuthEndpoint
import com.ditcalendar.bot.endpoint.CalendarEndpoint
import com.ditcalendar.bot.endpoint.TaskEndpoint
import com.elbekD.bot.types.User
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.flatMap
import com.github.kittinunf.result.map

class DitCalendarCommand {

    private val calendarEndpoint = CalendarEndpoint()
    private val taskEndpoint = TaskEndpoint()
    private val authEndpoint = AuthEndpoint()

    fun getCalendarAndTask(calendarId: Long): Result<String, Exception> {
        val calendarResult = calendarEndpoint.readCalendar(calendarId)
        val tokenResul = authEndpoint.getToken()
        val tasksResulst = tokenResul.flatMap { taskEndpoint.readTasks(calendarId, it) }

        return calendarResult.flatMap { calendar ->
            tasksResulst.map {
                calendar.apply { tasks = it }
            }
        }.map { it.toStringInMarkdown() + System.lineSeparator() }
    }

    fun assignUserToTask(taskId: Long, chatId: Long, user: User): Result<String, Exception> {
        val telegramLink = TelegramLink(chatId, user.id, user.username)
        return authEndpoint.getToken()
                .flatMap { taskEndpoint.assignUserToTask(taskId, telegramLink, it) }
    }
}