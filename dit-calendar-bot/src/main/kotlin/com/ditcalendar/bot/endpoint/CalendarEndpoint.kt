package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.data.DitCalendar
import com.github.kittinunf.fuel.core.extensions.authentication
import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.fuel.serialization.responseObject
import com.github.kittinunf.result.Result
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration

class CalendarEndpoint {

    private val config by config()

    private val json = Json(JsonConfiguration.Stable.copy(ignoreUnknownKeys = true))

    fun readCalendar(calendarId: Long, token: String): Result<DitCalendar, Exception> {

        val ditCalendarUrl = config[dit_calendar_server_url]

        return "$ditCalendarUrl/calendarentries/$calendarId"
                .httpGet()
                .authentication().bearer(token)
                .responseObject(loader = DitCalendar.serializer(), json = json)
                .third
    }
}