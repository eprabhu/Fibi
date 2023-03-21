package com.polus.fibicomp.triagequestionnaire.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.SQLException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.triagequestionnaire.pojo.TriageHeader;
import com.polus.fibicomp.triagequestionnaire.vo.TriageQuestionnaireVo;

import oracle.jdbc.OracleTypes;

@Service(value = "triageQuestionnaireDao")
public class TriageQuestionnaireDaoImpl implements TriageQuestionnaireDao {

	protected static Logger logger = LogManager.getLogger(TriageQuestionnaireDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public TriageHeader saveOrUpdateTriageHeader(TriageHeader triageHeader) {
		hibernateTemplate.saveOrUpdate(triageHeader);
		return triageHeader;
	}

	@Override
	public Integer evaluateTriageQuestionnaire(TriageQuestionnaireVo vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			String functionName = "FN_EVAL_TRIAGE_QUESTIONNAIRE";
			String functionCall = "{ ? = call " + functionName + "(?,?,?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setInt(2, Constants.AGREEMENT_MODULE_CODE);
			statement.setInt(3, Constants.TRIAGE_SUB_MODULE_CODE);
			statement.setString(4, vo.getTriageHeaderId().toString());
			statement.execute();
			return statement.getInt(1);
		} catch (SQLException e) {
			e.printStackTrace();
			return 0;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public Integer getModuleItemKeyBasedOnParms(Integer triageTemplateId, Integer moduleCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String maxHQLQuery = "SELECT moduleItemKey FROM TriageTemplateMapping WHERE triageTemplateId=:triageTemplateId and moduleCode = :moduleCode";
			Query<String> countQuery = session.createQuery(maxHQLQuery);
			countQuery.setParameter("triageTemplateId", triageTemplateId);
			countQuery.setParameter("moduleCode", moduleCode);
			return Integer.valueOf(countQuery.uniqueResult());
		} catch (Exception e) {
			return 0;
		}
	}

	@Override
	public TriageHeader getTriageHeaderById(Integer triageHeaderId) {
		return hibernateTemplate.get(TriageHeader.class, triageHeaderId);
		}
}
