package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.data.*
import com.github.kittinunf.fuel.core.extensions.authentication
import com.github.kittinunf.fuel.httpDelete
import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.fuel.httpPut
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.map
import kotlinx.serialization.builtins.list
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration

class TelegramAssignmentEndpoint {

    private val config by config()

    private val ditCalendarUrl = config[dit_calendar_server_url]

    private val json = Json(JsonConfiguration.Stable.copy(ignoreUnknownKeys = true))

    fun readTasks(calendarId: Long, token: String): Result<TelegramTaskAssignments, Exception> {

        val (_, _, result) = "$ditCalendarUrl/calendarentries/$calendarId/telegramlinks"
                .httpGet()
                .authentication().bearer(token)
                .responseString()

        return result.map { json.parse(TelegramTaskForAssignment.serializer().list, result.get()) }
    }

    fun assignUserToTask(taskId: Long, telegramLink: TelegramLink, token: String): Result<TelegramTaskForUnassignment, Exception> {

        val (_, _, result) = "$ditCalendarUrl/calendarentries/tasks/$taskId/assignment"
                .httpPut()
                .body(json.stringify(TelegramLink.serializer(), telegramLink))
                .authentication().bearer(token)
                .responseString()

        return result.map {
            json.parse(TelegramTaskForUnassignment.serializer(), result.get())
        }
    }

    fun unassignUserFromTask(taskId: Long, telegramLink: TelegramLink, token: String): Result<TelegramTaskAfterUnassignment, Exception> {
        val (_, _, result) = "$ditCalendarUrl/calendarentries/tasks/$taskId/assignment"
                .httpDelete()
                .body(json.stringify(TelegramLink.serializer(), telegramLink))
                .authentication().bearer(token)
                .responseString()

        return result.map { json.parse(TelegramTaskAfterUnassignment.serializer(), result.get()) }
    }


}