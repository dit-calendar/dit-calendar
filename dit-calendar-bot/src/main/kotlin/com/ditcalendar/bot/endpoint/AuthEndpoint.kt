package com.ditcalendar.bot.endpoint

import com.github.kittinunf.fuel.httpPost
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.flatMap
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import org.eclipse.microprofile.config.inject.ConfigProperty
import javax.enterprise.context.ApplicationScoped

@ApplicationScoped
class AuthEndpoint {


    @ConfigProperty(name = "dit.calendar.server.url")
    private lateinit var ditCalendarUrl: String

    @ConfigProperty(name = "dit.calendar.user.name")
    private lateinit var userName: String

    @ConfigProperty(name = "dit.calendar.user.password")
    private lateinit var password: String

    private val json = Json(JsonConfiguration.Stable.copy())

    fun getToken(): Result<String, Exception> {

        val (_, _, result) = "$ditCalendarUrl/authenticate/authentication-methods/password/token"
                .httpPost()
                .body("{\"user\":\"$userName\",\"password\":\"$password\"}")
                .responseString()


        return result.flatMap {
            Result.of<String, Exception> {
                val jwt = json.parse(JWT.serializer(), result.get())
                if (jwt.jrStatus == "Ok")
                    jwt.jrData.token
                else
                    throw Exception("jrStatus: ${jwt.jrStatus}")
            }
        }
    }

    @Serializable
    private data class JWT(val jrStatus: String, val jrData: Token) {
        @Serializable
        data class Token(val token: String)
    }
}