package com.polus.appcoigraph.config;

import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Config;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class Neo4jConnectionManager {
    private static final int MAX_POOL_SIZE = 10;
    private static Driver driver;

    @Value("${spring.neo4j.uri}")
    private String url;

    @Value("${spring.neo4j.authentication.username}")
    private String username;

    @Value("${spring.neo4j.authentication.password}")
    private String password;

    public Driver getDriver() {
        if (driver == null) {
            Config config = Config.builder().withMaxConnectionPoolSize(MAX_POOL_SIZE).build();
            driver = GraphDatabase.driver(url, AuthTokens.basic(username, password), config);
        }
        return driver;
    }

    public void closeDriver() {
        if (driver != null) {
            driver.close();
            driver = null;
        }
    }
}

