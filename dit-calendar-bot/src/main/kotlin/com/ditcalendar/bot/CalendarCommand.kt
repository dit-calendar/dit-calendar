package com.ditcalendar.bot

import com.ditcalendar.bot.endpoint.AuthEndpoint
import com.ditcalendar.bot.endpoint.CalendarEndpoint
import com.ditcalendar.bot.endpoint.TaskEndpoint

class CalendarCommand {

    private val calendarEndpoint = CalendarEndpoint()
    private val taskEndpoint = TaskEndpoint()
    private val authEndpoint = AuthEndpoint()

    fun getCalendarAndTask(calendarId: Long) : String? {
        val calendar = calendarEndpoint.readCalendar(calendarId) ?: return null
        val oauthToken = authEndpoint.getToken()
        val tasks = oauthToken?.let { taskEndpoint.readTasks(calendarId, oauthToken) }

        var response = calendar.toStringInMarkdown() + System.lineSeparator()
        tasks?.forEach { response += it.toStringInMarkdown() + System.lineSeparator()}

        return response
    }
}