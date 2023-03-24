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

public class ProposalIdGenerator implements IdentifierGenerator {

	protected static Logger logger = LogManager.getLogger(ProposalIdGenerator.class.getName());

	@Override
	public Serializable generate(SharedSessionContractImplementor sessionImplementor, Object object) throws HibernateException {
		String prefix = "A";
		Connection connection = sessionImplementor.connection();

		try {
			Statement statement = connection.createStatement();

			ResultSet rs = statement.executeQuery("select count(1) from EPS_PROPOSAL");

			if (rs.next()) {
				int id = rs.getInt(1) + 1001;
				String generatedId = prefix + "" + Integer.valueOf(id).toString();
				logger.info("Generated Application Id : " + generatedId);
				return generatedId;
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return null;
	}
}
