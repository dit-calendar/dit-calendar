package com.ditcalendar.bot.data

import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.data.core.DateSerializer
import kotlinx.serialization.Serializable
import java.util.*

typealias Tasks = List<Task>

@Serializable
sealed class Task : Base() {
    abstract val taskId: Long
    abstract val title: String
    abstract val description: String?

    @Serializable(with = DateSerializer::class)
    abstract val startTime: Date

    @Serializable(with = DateSerializer::class)
    abstract val endTime: Date?
}

@Serializable
class TaskForAssignment(override val taskId: Long,
                        override val title: String,
                        override val description: String? = null,
                        @Serializable(with = DateSerializer::class)
                        override val startTime: Date,
                        @Serializable(with = DateSerializer::class)
                        override val endTime: Date? = null) : Task()

@Serializable
class TaskForUnassignment(override val taskId: Long,
                          override val title: String,
                          override val description: String? = null,
                          @Serializable(with = DateSerializer::class)
                          override val startTime: Date,
                          @Serializable(with = DateSerializer::class)
                          override val endTime: Date? = null) : Task()

@Serializable
class TaskAfterUnassignment(override val taskId: Long,
                            override val title: String,
                            override val description: String? = null,
                            @Serializable(with = DateSerializer::class)
                            override val startTime: Date,
                            @Serializable(with = DateSerializer::class)
                            override val endTime: Date? = null) : Task()