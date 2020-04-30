package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.data.*
import com.github.kittinunf.fuel.core.extensions.authentication
import com.github.kittinunf.fuel.httpDelete
import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.fuel.httpPut
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.flatMap
import kotlinx.serialization.builtins.list
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import org.eclipse.microprofile.config.inject.ConfigProperty
import javax.enterprise.context.ApplicationScoped

@ApplicationScoped
class TaskEndpoint {

    @ConfigProperty(name = "dit.calendar.server.url")
    private lateinit var ditCalendarUrl: String

    private val json = Json(JsonConfiguration.Stable.copy(ignoreUnknownKeys = true))

    fun readTasks(calendarId: Long, token: String): Result<Tasks, Exception> {

        val (_, _, result) = "$ditCalendarUrl/calendarentries/$calendarId/tasks"
                .httpGet()
                .authentication().bearer(token)
                .responseString()

        return result.flatMap {
            Result.of<Tasks, Exception> {
                json.parse(TaskForAssignment.serializer().list, result.get())
            }
        }
    }

    fun assignUserToTask(taskId: Long, telegramLink: TelegramLink, token: String): Result<TaskForUnassignment, Exception> {

        val (_, _, result) = "$ditCalendarUrl/calendarentries/tasks/$taskId/assignment"
                .httpPut()
                .body(json.stringify(TelegramLink.serializer(), telegramLink))
                .authentication().bearer(token)
                .responseString()

        return result.flatMap {
            Result.of<TaskForUnassignment, Exception> {
                json.parse(TaskForUnassignment.serializer(), result.get())
            }
        }
    }

    fun unassignUserFromTask(taskId: Long, telegramLink: TelegramLink, token: String): Result<TaskAfterUnassignment, Exception> {
        val (_, _, result) = "$ditCalendarUrl/calendarentries/tasks/$taskId/assignment"
                .httpDelete()
                .body(json.stringify(TelegramLink.serializer(), telegramLink))
                .authentication().bearer(token)
                .responseString()

        return result.flatMap {
            Result.of<TaskAfterUnassignment, Exception> {
                json.parse(TaskAfterUnassignment.serializer(), result.get())
            }
        }
    }


}