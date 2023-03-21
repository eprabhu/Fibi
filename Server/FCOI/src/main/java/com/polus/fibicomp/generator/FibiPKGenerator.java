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

public class FibiPKGenerator implements IdentifierGenerator {

	protected static Logger logger = LogManager.getLogger(FibiPKGenerator.class.getName());

	@Override
	public Serializable generate(SharedSessionContractImplementor session, Object obj) throws HibernateException {
		String identifierPropertyName = session.getEntityPersister(obj.getClass().getName(), obj).getIdentifierPropertyName();
		logger.info("identifierPropertyName : " + identifierPropertyName);
		String simpleName = obj.getClass().getSimpleName();
		logger.info("simpleName : " + simpleName);
		String query = "";
		if (simpleName.equals("Proposal")) {
			query = "SELECT MAX(PROPOSAL_ID) FROM EPS_PROPOSAL";
		} else if (simpleName.equals("BudgetHeader")) {
			query = "SELECT MAX(BUDGET_HEADER_ID) FROM BUDGET_HEADER";
		} else if (simpleName.equals("FibiProposalRate")) {
			query = "SELECT MAX(PROPOSAL_RATE_ID) FROM EPS_PROPOSAL_RATES";
		} else if (simpleName.equals("Workflow")) {
			query = "SELECT MAX(WORKFLOW_ID) FROM WORKFLOW";
		} else if (simpleName.equals("WorkflowDetail")) {
			query = "SELECT MAX(WORKFLOW_DETAIL_ID) FROM WORKFLOW_DETAIL";
		} else if (simpleName.equals("ProposalPreReview")) {
			query = "SELECT MAX(PRE_REVIEW_ID) FROM EPS_PROP_PRE_REVIEW";
		} else if (simpleName.equals("NotificationType")) {
			query = "SELECT MAX(NOTIFICATION_TYPE_ID) FROM NOTIFICATION_TYPE";
		} else if (simpleName.equals("AwardBudgetHeader")) {
			query = "SELECT MAX(BUDGET_HEADER_ID) FROM AWARD_BUDGET_HEADER";
		} else if (simpleName.equals("AwardRates")) {
			query = "SELECT MAX(AWARD_RATE_ID) FROM AWARD_RATES";
		} else if (simpleName.equals("ServiceRequest")) {
			query = "SELECT MAX(SR_HEADER_ID) FROM SR_HEADER";
		} else if (simpleName.equals("Role")) {
			query = "SELECT MAX(ROLE_ID) FROM ROLE";
		}

		logger.info("query : " + query);
		Connection connection = session.connection();

		try {
			Statement statement = connection.createStatement();

			ResultSet rs = statement.executeQuery(query);

			if (rs.next()) {
				int id = rs.getInt(1) + 1;
				logger.info("Generated Id: " + id);
				return id;
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return null;
	}

}
