package com.polus;

import java.beans.PropertyVetoException;
import java.util.Properties;

import javax.sql.DataSource;

import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.orm.hibernate5.HibernateTransactionManager;
import org.springframework.orm.hibernate5.LocalSessionFactoryBean;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.mchange.v2.c3p0.ComboPooledDataSource;

import jakarta.persistence.EntityManagerFactory;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(basePackages = "com.polus.*", entityManagerFactoryRef = "entityManagerFactory", transactionManagerRef = "transactionManager")
public class FibiRepoConfig extends HibernateJpaAutoConfiguration {

	@Value("${spring.datasource.driverClassName}")
	private String driverClassName;

	@Value("${spring.datasource.url}")
	private String url;

	@Value("${spring.datasource.username}")
	private String username;

	@Value("${spring.datasource.password}")
	private String password;

	@Value("${spring.jpa.database-platform}")
	private String hibernateDialect;

	@Value("${spring.jpa.properties.hibernate.show_sql}")
	private String hibernateShowSql;

	@Value("${spring.jpa.properties.hibernate.hbm2ddl.auto}")
	private String hibernateHbm2ddlAuto;

	@Value("${spring.jpa.properties.hibernate.format_sql}")
	private String hibernateFormatSql;

	@Value("${spring.jpa.properties.hibernate.c3p0.minPoolSize}")
	private String hibernateMinPoolSize;

	@Value("${spring.jpa.properties.hibernate.c3p0.maxPoolSize}")
	private String hibernateMaxPoolSize;

	@Value("${spring.jpa.properties.hibernate.c3p0.timeout}")
	private String hibernateTimeOut;

	@Value("${spring.jpa.properties.hibernate.c3p0.max_statement}")
	private String hibernateMaxStmnt;

	@Value("${spring.jpa.properties.hibernate.c3p0.testConnectionOnCheckout}")
	private String hibernateTestConnectionOnCheckout;

	@Value("${spring.jpa.properties.hibernate.jdbc.lob.non_contextual_creation}")
	private String hibernateContextualCreation;

	@Value("${spring.jpa.properties.hibernate.temp.use_jdbc_metadata_defaults}")
	private String hibernateSpringJDBCMetadataDefault;

	@Value("${spring.jpa.properties.hibernate.default_schema}")
	private String hibernateDefaultSchema;

	@Bean
	@Primary
	EntityManagerFactory entityManagerFactory() {
		LocalContainerEntityManagerFactoryBean factory = new LocalContainerEntityManagerFactoryBean();
		factory.setDataSource(getDataSource());
		factory.setPackagesToScan("com.polus.*");
		factory.setJpaVendorAdapter(new HibernateJpaVendorAdapter());
		factory.setJpaProperties(getHibernateProperties());
		factory.afterPropertiesSet();
		return factory.getObject();
	}

	@Bean
	DataSource getDataSource() {
		ComboPooledDataSource dataSource = new ComboPooledDataSource();
		dataSource.setJdbcUrl(url);
		dataSource.setUser(username);
		dataSource.setPassword(password);
		dataSource.setDataSourceName("fibids");
		try {
			dataSource.setDriverClass(driverClassName);
		} catch (PropertyVetoException e) {
			e.printStackTrace();
		}
		dataSource.setMinPoolSize(3);
		dataSource.setMaxPoolSize(20);
		dataSource.setAcquireIncrement(1);
		dataSource.setIdleConnectionTestPeriod(3000);
		dataSource.setMaxIdleTime(600);
		dataSource.setMaxStatements(0);
		dataSource.setTestConnectionOnCheckout(true);
		return dataSource;
	}

	@Bean
	@Primary
	HibernateTransactionManager hibernateTransactionManager() {
		HibernateTransactionManager transactionManager = new HibernateTransactionManager(sessionFactory());
		return transactionManager;
	}

	@Bean
	SessionFactory sessionFactory() {
		LocalSessionFactoryBean sessionFactoryBean = new LocalSessionFactoryBean();
		sessionFactoryBean.setDataSource(getDataSource());
		sessionFactoryBean.setPackagesToScan("com.polus.*");
		sessionFactoryBean.setHibernateProperties(getHibernateProperties());
		try {
			sessionFactoryBean.afterPropertiesSet();
		} catch (Exception e) {
			throw new RuntimeException("Failed to configure sessionFactory: " + e.getMessage(), e);
		}
		return sessionFactoryBean.getObject();
	}

	@Bean
	Properties getHibernateProperties() {
		Properties properties = new Properties();
		properties.put("hibernate.dialect", hibernateDialect);
		properties.put("hibernate.show_sql", hibernateShowSql);
		properties.put("hibernate.format_sql", hibernateFormatSql);
		properties.put("hibernate.c3p0.minPoolSize", hibernateMinPoolSize);
		properties.put("hibernate.c3p0.maxPoolSize", hibernateMaxPoolSize);
		properties.put("hibernate.c3p0.timeout", hibernateTimeOut);
		properties.put("hibernate.c3p0.max_statements", hibernateMaxStmnt);
		properties.put("hibernate.c3p0.testConnectionOnCheckout", hibernateTestConnectionOnCheckout);
		properties.put("hibernate.jdbc.lob.non_contextual_creation", hibernateContextualCreation);
		properties.put("hibernate.temp.use_jdbc_metadata_defaults", hibernateSpringJDBCMetadataDefault);
		properties.put("hibernate.default_schema", hibernateDefaultSchema);
		return properties;
	}

	@Bean
	DataSourceTransactionManager transactionManager(DataSource dataSource) {
		return new DataSourceTransactionManager(dataSource);
	}

}
