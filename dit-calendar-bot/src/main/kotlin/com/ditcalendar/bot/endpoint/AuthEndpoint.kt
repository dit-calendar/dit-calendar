package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.config.dit_calendar_user_name
import com.ditcalendar.bot.config.dit_calendar_user_password
import com.github.kittinunf.fuel.httpPost
import com.github.kittinunf.fuel.serialization.responseObject
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.map
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration


class AuthEndpoint {

    private val config by config()

    private val ditCalendarUrl = config[dit_calendar_server_url]
    private val userName = config[dit_calendar_user_name]
    private val password = config[dit_calendar_user_password]

    private val json = Json(JsonConfiguration.Stable.copy())

    fun getToken(): Result<String, Exception> {

        val (_, _, result) = "$ditCalendarUrl/authenticate/authentication-methods/password/token"
                .httpPost()
                .body("{\"user\":\"$userName\",\"password\":\"$password\"}")
                .responseObject(loader = JWT.serializer(), json = json)

        return result.map { jwt ->
            if (jwt.jrStatus == "Ok")
                jwt.jrData.token
            else
                throw Exception("jrStatus: ${jwt.jrStatus}")
        }
    }

    @Serializable
    private data class JWT(val jrStatus: String, val jrData: Token) {
        @Serializable
        data class Token(val token: String)
    }
}