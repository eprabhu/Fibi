package com.polus.fibicomp.migration.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.SQLException;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.transaction.Transactional;

import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.polus.fibicomp.migration.pojo.MigrationAttachmentErrorLog;
import com.polus.fibicomp.migration.pojo.TempAttachmentMigration;

import oracle.jdbc.internal.OracleTypes;

@Transactional
@Service(value = "migrationDao")
public class MigrationDaoImpl implements MigrationDao {

	@Autowired
	public HibernateTemplate hibernateTemplate;
	
	@Value("${oracledb}")
	private String oracledb;
	
	protected static Logger logger = LogManager.getLogger(MigrationDaoImpl.class.getName());

	@Override
	public void saveOrUpdateMigrationData(TempAttachmentMigration tempAttachmentMigration) {
		try {
			hibernateTemplate.saveOrUpdate(tempAttachmentMigration);
		} catch (Exception e) {
			e.getMessage();
		}
	}

	@Override
	public boolean checkFileAlreadyExist(String fileName, String projectId, String attachmentType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<TempAttachmentMigration> query = builder.createQuery(TempAttachmentMigration.class);
		Root<TempAttachmentMigration> rootTempAttachmentMigration = query.from(TempAttachmentMigration.class);
		query.select(rootTempAttachmentMigration.get("id"));
		Predicate predicate1 = builder.equal(rootTempAttachmentMigration.get("fileName"), fileName);
		Predicate predicate2 = builder.equal(rootTempAttachmentMigration.get("projectId"), projectId);
		Predicate predicate3 = builder.equal(rootTempAttachmentMigration.get("attachmentType"), attachmentType);
		query.where(builder.and(predicate1, predicate2, predicate3));
		if (session.createQuery(query).getResultList().isEmpty()) {
			return true;
		} else {
			return false;
		}
	}
	
	@Override
	public void migrationAttachmentFeed(String wbsNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call MIGRATION_ATTACHMENT_FEED(?)}");
				statement.setString(1, wbsNumber);
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "MIGRATION_ATTACHMENT_FEED";
				String functionCall = "{call " + procedureName + "(?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setString(1, wbsNumber);
				statement.registerOutParameter(2, OracleTypes.CURSOR);
				statement.executeUpdate();
				statement.getObject(2);
			}
		} catch (Exception e) {
			logger.error("Exception in migrationAttachmentFeed : {}", e.getMessage());
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Exception in migrationAttachmentFeed : {}", e.getMessage());
			}
		}
	}

	@Override
	public void saveErrorLog(MigrationAttachmentErrorLog migrationAttachmentErrorLog) {
		hibernateTemplate.save(migrationAttachmentErrorLog);
	}

	@Override
	public Integer getMaxId() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<MigrationAttachmentErrorLog> query = builder.createQuery(MigrationAttachmentErrorLog.class);
		Root<MigrationAttachmentErrorLog> rootMigrationAttachmentErrorLog = query.from(MigrationAttachmentErrorLog.class);
		query.select(rootMigrationAttachmentErrorLog.get("id"));
		return session.createQuery(query).getMaxResults();
	}

	@Override
	public void grantCallAttachmentDataMigrationFeed(Integer avType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call MIGRATION_GRANTCALL_ATTACHMENT_FEED(?)}");
				statement.setInt(1, avType);
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "MIGRATION_GRANTCALL_ATTACHMENT_FEED";
				String functionCall = "{call " + procedureName + "(?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setInt(1, avType);
				statement.registerOutParameter(2, OracleTypes.CURSOR);
				statement.executeUpdate();
				statement.getObject(2);
			}
		} catch (Exception e) {
			logger.error("Exception in grantCallAttachmentDataMigrationFeed : {}", e.getMessage());
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Exception in migrationAttachmentFeed : {}", e.getMessage());
			}
		}
	}

}
