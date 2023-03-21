package com.polus.fibicomp.search.service;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.HashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "searchService")
public class SearchServiceImpl implements SearchService {

	protected static Logger logger = LogManager.getLogger(SearchServiceImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public String searchDocument(Integer moduleCode, String searchText) {
		ArrayList<HashMap<String, Object>> result = search(moduleCode, searchText);
		return commonDao.convertToJSON(result);
	}

	private ArrayList<HashMap<String, Object>> search(Integer moduleCode, String searchText) {

		if (Constants.AWARD_MODULE_CODE.equals(moduleCode)) {
			ResultSet resultSet = searchInDatabase(moduleCode, searchText);
			return bindAwardResult(resultSet);

		} else if (Constants.INSTITUTE_PROPOSAL_MODULE_CODE.equals(moduleCode)) {
			ResultSet resultSet = searchInDatabase(moduleCode, searchText);
			return bindIPResult(resultSet);

		} else if (Constants.DEV_PROPOSAL_MODULE_CODE.equals(moduleCode)) {
			ResultSet resultSet = searchInDatabase(moduleCode, searchText);
			return bindDevProposalResult(resultSet);
		}

		return null;
	}

	private ResultSet searchInDatabase(Integer moduleCode, String searchText) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "GET_ES_PROJECT_SEARCH";
				String functionCall = "{call " + procedureName + "(?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setInt(1, moduleCode);
				statement.setString(2, searchText);
				statement.registerOutParameter(3, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(3);
			} else if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call GET_ES_PROJECT_SEARCH(?,?)}");
				statement.setInt(1, moduleCode);
				statement.setString(2, searchText);
				statement.execute();
				resultSet = statement.getResultSet();
			}
		} catch (Exception e) {
			logger.error("ERROR IN searchInDatabase Method:" + e);
		}
		return resultSet;
	}

	private ArrayList<HashMap<String, Object>> bindIPResult(ResultSet resultSet) {
		ArrayList<HashMap<String, Object>> results = new ArrayList<>();
		try {
			if (resultSet != null) {
				while (resultSet.next()) {
					HashMap<String, Object> result = new HashMap<String, Object>();
					result.put("proposal_id", resultSet.getString("PROPOSAL_ID"));
					result.put("proposal_number", resultSet.getString("PROPOSAL_NUMBER"));
					result.put("title", resultSet.getString("TITLE"));
					result.put("lead_unit_number", resultSet.getString("LEAD_UNIT_NUMBER"));
					result.put("status", resultSet.getString("STATUS"));
					result.put("lead_unit_name", resultSet.getString("LEAD_UNIT_NAME"));
					result.put("sponsor", resultSet.getString("SPONSOR"));
					result.put("status_code", resultSet.getString("STATUS_CODE"));
					results.add(result);

				}
			}
		} catch (Exception e) {
			logger.error("ERROR IN bindIPResult Method:" + e);
		}

		return results;
	}

	private ArrayList<HashMap<String, Object>> bindAwardResult(ResultSet resultSet) {
		ArrayList<HashMap<String, Object>> results = new ArrayList<>();
		try {
			if (resultSet != null) {
				while (resultSet.next()) {
					HashMap<String, Object> result = new HashMap<String, Object>();
					// To Do
					results.add(result);

				}
			}
		} catch (Exception e) {
			logger.error("ERROR IN bindAwardResult Method:" + e);
		}

		return results;
	}

	private ArrayList<HashMap<String, Object>> bindDevProposalResult(ResultSet resultSet) {
		ArrayList<HashMap<String, Object>> results = new ArrayList<>();
		try {
			if (resultSet != null) {
				while (resultSet.next()) {
					HashMap<String, Object> result = new HashMap<String, Object>();
					// To Do
					results.add(result);

				}
			}
		} catch (Exception e) {
			logger.error("ERROR IN bindDevProposalResult Method:" + e);
		}

		return results;
	}
}
