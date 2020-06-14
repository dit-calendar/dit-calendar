import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "dit-calendar"
version = "0.7.0.0-SNAPSHOT"

plugins {
    val kotlinVersion = "1.3.72"

    application

    kotlin("jvm") version kotlinVersion
    kotlin("plugin.serialization") version kotlinVersion

    id("com.github.johnrengelman.shadow") version "5.2.0"
}

application {
    mainClassName = "com.ditcalendar.bot.BotKt"
}

repositories {
    mavenLocal()
    mavenCentral()
    maven { url = uri("https://jitpack.io") }
    maven { url = uri("https://kotlin.bintray.com/kotlinx") }
}

dependencies {
    val fuelVersion = "2.2.1"
    val kittinunfResultVersion = "3.0.0"
    val konfigVersion = "1.6.10.0"
    val kotlinxSerializationVersion = "0.20.0"
    val ktBotVersion = "1.3.3"

    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    implementation("com.github.kittinunf.fuel:fuel:$fuelVersion")
    implementation("com.github.kittinunf.fuel:fuel-kotlinx-serialization:$fuelVersion")
    implementation("com.github.kittinunf.fuel:fuel-coroutines:$fuelVersion")
    implementation("com.github.kittinunf.result:result:$kittinunfResultVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-runtime:$kotlinxSerializationVersion")
    implementation("com.github.elbekd:kt-telegram-bot:$ktBotVersion")
    implementation("com.natpryce:konfig:$konfigVersion")
}

tasks.withType<KotlinCompile>().configureEach {
    sourceCompatibility = "11"
    kotlinOptions.jvmTarget = "11"

    kotlinOptions.freeCompilerArgs = listOf("-Xjsr305=strict")
    incremental = true
}