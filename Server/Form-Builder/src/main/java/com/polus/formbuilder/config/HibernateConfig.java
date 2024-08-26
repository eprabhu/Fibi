package com.polus.formbuilder.config;

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
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.mchange.v2.c3p0.ComboPooledDataSource;

import jakarta.persistence.EntityManagerFactory;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(basePackages = "com.polus.*", entityManagerFactoryRef = "entityManagerFactory", transactionManagerRef = "transactionManager")

 
public class HibernateConfig extends HibernateJpaAutoConfiguration {

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
	private String hibernateDefault_schema;
	
	//HikariCP properties
	@Value("${spring.datasource.hikari.connectionTimeout}")
	private Long hikariConnectionTimeout;
	
	@Value("${spring.datasource.hikari.maximumPoolSize}")
	private int hikariMaximumPoolSize;
		
	@Value("${spring.datasource.hikari.idleTimeout}")
	private int hikariIdleTimeout;
	
	@Bean
	@Primary
	EntityManagerFactory entityManagerFactory() {
		LocalContainerEntityManagerFactoryBean factory = new LocalContainerEntityManagerFactoryBean();
		factory.setDataSource(getDataSource());
		factory.setPackagesToScan("com.*");
		factory.setJpaVendorAdapter(new HibernateJpaVendorAdapter());
		factory.setJpaProperties(getHibernateProperties());
		factory.afterPropertiesSet();
		return factory.getObject();
	}

//	@Bean
//	public DataSource getDataSource() {
//		DriverManagerDataSource driverManagerDataSource = new DriverManagerDataSource();
//		driverManagerDataSource.setDriverClassName(driverClassName);
//		driverManagerDataSource.setUrl(url);
//		driverManagerDataSource.setUsername(username);
//		driverManagerDataSource.setPassword(password);			
//		return driverManagerDataSource;
//	}
	
	//C3P0 cP
	@Bean
	public DataSource getDataSource() {
	ComboPooledDataSource dataSource = new ComboPooledDataSource();
    dataSource.setJdbcUrl(url);
    dataSource.setUser(username);
    dataSource.setPassword(password);
    dataSource.setDataSourceName("fibids");
    try {
		dataSource.setDriverClass(driverClassName);
	} catch (PropertyVetoException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();		}

    // Configure other C3P0 properties as needed
    dataSource.setMinPoolSize(10);
    dataSource.setMaxPoolSize(50);
    dataSource.setAcquireIncrement(1);
    dataSource.setIdleConnectionTestPeriod(1200);
    dataSource.setMaxIdleTime(600);
    dataSource.setMaxStatements(10);
    dataSource.setTestConnectionOnCheckout(true);
    return dataSource;
}
//	  @Bean
//    public DataSource getDataSource() {    	
//    	HikariDataSource dataSource = new HikariDataSource();
//    	dataSource.setJdbcUrl(url);
//        dataSource.setUsername(username);
//        dataSource.setPassword(password);
//        dataSource.setMaximumPoolSize(hikariMaximumPoolSize);
//        dataSource.setMinimumIdle(hikariIdleTimeout);
//        dataSource.setConnectionTimeout(hikariConnectionTimeout);
//    	return dataSource;
//    	
//    }
	
	@Bean
    @Primary
    public HibernateTransactionManager hibernateTransactionManager() {
        HibernateTransactionManager transactionManager = new HibernateTransactionManager(sessionFactory());
        return transactionManager;
    }	
//	
//	@Bean
//	LocalSessionFactoryBean sessionFactory() {
//		LocalSessionFactoryBean sessionFactory = new LocalSessionFactoryBean();
//		sessionFactory.setDataSource(dataSource);
//		sessionFactory.setPackagesToScan("com.polus.appcorelib.*"); // Package where your entity classes reside
//		sessionFactory.setHibernateProperties(getHibernateProperties());
//		return sessionFactory;
//	}
	
	@Bean
    public SessionFactory sessionFactory() {
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
	
	
	
	
	public Properties getHibernateProperties() {
		Properties properties = new Properties();
		properties.put("hibernate.dialect", hibernateDialect);
		properties.put("hibernate.show_sql", hibernateShowSql);
		properties.put("hibernate.format_sql", hibernateFormatSql);
		
		properties.put("hibernate.c3p0.minPoolSize", hibernateMinPoolSize);
		properties.put("hibernate.c3p0.maxPoolSize", hibernateMaxPoolSize);
		properties.put("hibernate.c3p0.timeout", hibernateTimeOut);
		properties.put("hibernate.c3p0.max_statement", hibernateMaxStmnt);
		properties.put("hibernate.c3p0.testConnectionOnCheckout", hibernateTestConnectionOnCheckout);
	
		properties.put("hibernate.jdbc.lob.non_contextual_creation", hibernateContextualCreation);
		properties.put("spring.jpa.properties.hibernate.temp.use_jdbc_metadata_defaults",
				hibernateSpringJDBCMetadataDefault);
		properties.put("hibernate.default_schema", hibernateDefault_schema);
		return properties;
	}
	

    @Bean
    public DataSourceTransactionManager transactionManager(DataSource dataSource) {
        return new DataSourceTransactionManager(dataSource);
    }

	@Bean
	HibernateTemplate hibernateTemplate() {
		return new HibernateTemplate(sessionFactory());
	}

}