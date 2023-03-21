package com.polus.fibicomp.persontraining.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.persontraining.pojo.PersonTraining;
import com.polus.fibicomp.persontraining.pojo.PersonTrainingAttachment;
import com.polus.fibicomp.persontraining.pojo.PersonTrainingComment;
import com.polus.fibicomp.persontraining.pojo.Training;
import com.polus.fibicomp.person.vo.PersonVO;

import oracle.jdbc.OracleTypes;

@Transactional
@Service
public class PersonTrainingDaoImpl implements PersonTrainingDao {
	
	protected static Logger logger = LogManager.getLogger(PersonTrainingDaoImpl.class.getName());
	
	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;
	
	@Override
	public void saveOrUpdatePersonTraining(PersonTraining personTraining) {
		hibernateTemplate.saveOrUpdate(personTraining);
	}

	@Override
	public void saveOrUpdateTrainingComments(PersonTrainingComment personTrainingComment) {
		hibernateTemplate.saveOrUpdate(personTrainingComment);
	}

	@Override
	public void deleteTrainingComments(Integer trainingCommentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<PersonTrainingComment> query = builder.createCriteriaDelete(PersonTrainingComment.class);
		Root<PersonTrainingComment> root = query.from(PersonTrainingComment.class);
		query.where(builder.equal(root.get("trainingCommentId"), trainingCommentId));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public PersonTraining getPersonTraining(Integer trainingId) {
		return hibernateTemplate.get(PersonTraining.class, trainingId);
	}

	@Override
	public PersonVO getTrainingDashboard(PersonVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;	
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		Map<String, String> sort = vo.getSort();
		List<PersonTraining> persontrainingList = new ArrayList<>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_TRAINING_DASHBOARD(?,?,?,?,?,?)}");
				statement.setInt(1, vo.getTrainingCode() == null ? 0 : vo.getTrainingCode());
				statement.setString(2, vo.getPersonId());
				statement.setString(3, vo.getProperty1());
				statement.setString(4, setTrainingSortOrder(sort));
				statement.setInt(5, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(6, (pageNumber == null ? 0 : pageNumber));
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_TRAINING_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, vo.getTrainingCode());
				statement.setString(3, vo.getPersonId());
				statement.setString(4, vo.getProperty1());
				statement.setString(5, setTrainingSortOrder(sort));
				statement.setInt(6, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(7, (pageNumber == null ? 0 : pageNumber));
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				PersonTraining training = new PersonTraining();
				training.setPersonTrainingId(resultSet.getInt("PERSON_TRAINING_ID"));
				training.setTrainingCode(resultSet.getInt("TRAINING_CODE"));
				training.setTrainingDescription(resultSet.getString("DESCRIPTION"));
				training.setPersonId(resultSet.getString("PERSON_ID"));
				training.setUpdateTimestamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				training.setPersonName(resultSet.getString("FULL_NAME"));
				training.setNonEmployee(resultSet.getBoolean("IS_NON_EMPLOYEE"));
				training.setFollowupDate(resultSet.getTimestamp("FOLLOWUP_DATE"));
				training.setDateAcknowledged(resultSet.getTimestamp("DATE_ACKNOWLEDGED"));
				persontrainingList.add(training);
			}
			vo.setTrainings(persontrainingList);
			vo.setTotalResult(getTrainingDashboardCount(vo));
		} catch (SQLException e) {
			logger.error("Error in getTrainingDashboard {}", e.getMessage());
		} finally {
			try {
				statement.close();
			} catch (Exception e2) {
				logger.error("Error in getTrainingDashboard statement close {}", e2.getMessage());
			}
		}
		return vo;
	}

	private Integer getTrainingDashboardCount(PersonVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_TRAINING_DASHBOARD_COUNT(?,?,?,?,?,?)}");
				statement.setInt(1, vo.getTrainingCode() == null ? 0 : vo.getTrainingCode());
				statement.setString(2, vo.getPersonId());
				statement.setString(3, vo.getProperty1());
				statement.setString(4, setTrainingSortOrder(sort));
				statement.setInt(5, 0);
				statement.setInt(6, 0);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_TRAINING_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, vo.getTrainingCode());
				statement.setString(3, vo.getPersonId());
				statement.setString(4, vo.getProperty1());
				statement.setString(5, setTrainingSortOrder(sort));
				statement.setInt(6, 0);
				statement.setInt(7, 0);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.error("Error in getTrainingDashboardCount {}", e.getMessage());
		} finally {
			try {
				statement.close();
			} catch (Exception e2) {
				logger.error("Error in getTrainingDashboard statement close {}", e2.getMessage());
			}
		}
		return count;
	}
	
	private String setTrainingSortOrder(Map<String, String> sort) {
		String sortOrder = null;	
		for (Map.Entry<String, String> mapElement : sort.entrySet()) {
			switch (mapElement.getKey()) {
			case "personName":
				sortOrder = (sortOrder == null ? "T.FULL_NAME " + mapElement.getValue() : sortOrder + ", T.FULL_NAME " + mapElement.getValue());
				break;
			case "trainingDescription":
				sortOrder = (sortOrder == null ? "T.DESCRIPTION " + mapElement.getValue() : sortOrder + ", T.DESCRIPTION " + mapElement.getValue());
				break;
			case "followupDate":
				sortOrder = (sortOrder == null ? "T.FOLLOWUP_DATE " + mapElement.getValue() : sortOrder + ", T.FOLLOWUP_DATE " + mapElement.getValue());
				break;
			default:
				break;
			}
		}
		return sortOrder;
	}

	@Override
	public List<Training> loadTrainingList(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Training> query = builder.createQuery(Training.class);
		Root<Training> training = query.from(Training.class);
		query.where(builder.like(training.get("description"), "%" + searchString + "%"));
		query.orderBy(builder.asc(training.get("description")));
		return session.createQuery(query).setMaxResults(20).getResultList();
	}

	@Override
	public void saveOrUpdateTrainingAttachment(PersonTrainingAttachment attachment) {
		hibernateTemplate.saveOrUpdate(attachment);	
	}

	@Override
	public PersonTrainingAttachment loadPersonTrainingAttachment(int attachmentId) {
		return hibernateTemplate.get(PersonTrainingAttachment.class, attachmentId);
	}

	@Override
	public void deleteTrainingAttachment(Integer trainingAttachmentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<PersonTrainingAttachment> delete = builder.createCriteriaDelete(PersonTrainingAttachment.class);
		Root<PersonTrainingAttachment> root = delete.from(PersonTrainingAttachment.class);
		delete.where(builder.equal(root.get("trainingAttachmentId"), trainingAttachmentId));
		session.createQuery(delete).executeUpdate();	
	}

	@Override
	public void deletePersonTraining(Integer personTrainingId) {
		PersonTraining persontraining = hibernateTemplate.get(PersonTraining.class, personTrainingId);
		if (persontraining != null) {
			hibernateTemplate.delete(persontraining);	
		}
	}
}
