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

/**
 * @author Shaji P
 *
 */
public class AwardNumberGenerator implements IdentifierGenerator {

	protected static Logger logger = LogManager.getLogger(AwardNumberGenerator.class.getName());

	@Override
	public Serializable generate(SharedSessionContractImplementor sessionImplementor, Object object) throws HibernateException {
		Connection connection = sessionImplementor.connection();
		try {
			Statement statement = connection.createStatement();

			ResultSet rs = statement.executeQuery("select SEQ_AWARD_NUMBER.next from dual");
			if (rs.next()) {
				int id = rs.getInt(1) + 000001;
				String generatedId = Integer.valueOf(id).toString() + "-" + "00001";
				logger.info("Generated Application Number : " + generatedId);
				return generatedId;
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return null;
	}

}
