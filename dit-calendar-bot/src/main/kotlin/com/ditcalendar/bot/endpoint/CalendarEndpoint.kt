package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.data.DitCalendar
import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.flatMap
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import org.eclipse.microprofile.config.inject.ConfigProperty
import javax.enterprise.context.ApplicationScoped

@ApplicationScoped
class CalendarEndpoint {

    @ConfigProperty(name = "dit.calendar.server.url")
    private lateinit var ditCalendarUrl: String

    private val json = Json(JsonConfiguration.Stable.copy(ignoreUnknownKeys = true))

    fun readCalendar(calendarId: Long): Result<DitCalendar, Exception> {

        val (_, _, result) = "$ditCalendarUrl/calendarentries/$calendarId"
                .httpGet()
                .responseString()

        return result.flatMap {
            Result.of<DitCalendar, Exception> {
                json.parse(DitCalendar.serializer(), result.get())
            }
        }
    }
}