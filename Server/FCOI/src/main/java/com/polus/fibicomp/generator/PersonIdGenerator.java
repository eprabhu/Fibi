package com.polus.fibicomp.generator;

import java.io.Serializable;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.id.IdentifierGenerator;

public class PersonIdGenerator implements IdentifierGenerator{
	
	protected static Logger logger = LogManager.getLogger(AwardNumberGenerator.class.getName());

	@Override
	public Serializable generate(SharedSessionContractImplementor sessionImplementor, Object object) throws HibernateException {
		String prefix = "99999900000";
		String generatedId = new String();
		Connection connection = sessionImplementor.connection();
		try {
			Statement statement = connection.createStatement();

			ResultSet rs = statement.executeQuery("select max(cast(person_id as SIGNED)) from PERSON where cast(person_id as SIGNED) > cast('99999900001'as SIGNED) and  person_id <> 'admin'");

			if (rs.next()) {
				Long id = rs.getLong(1) + 1;
				if(id == 1) {
				 generatedId = prefix + "" + Long.valueOf(id).toString();
				}
				else {
				 generatedId =  Long.valueOf(id).toString();
				}
				logger.info("Generated Person Id : " + generatedId);
				return generatedId;
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return null;
	}

}
