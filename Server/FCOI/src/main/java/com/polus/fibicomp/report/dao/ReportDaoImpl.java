package com.polus.fibicomp.report.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.report.pojo.ReportColumns;
import com.polus.fibicomp.report.pojo.ReportTemplate;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "reportDao")
public class ReportDaoImpl implements ReportDao {

	protected static Logger logger = LogManager.getLogger(ReportDaoImpl.class.getName());

	private static final String TEMPLATE_TYPE = "templateType";

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private DBEngine dbEngine;

	@Autowired
	private CommonDao commonDao;

	@Override
	public ReportTemplate saveOrUpdateReportTemplate(ReportTemplate reportTemplate) {
		hibernateTemplate.saveOrUpdate(reportTemplate);
		return reportTemplate;
	}

	@Override
	public ReportTemplate getReportTemplateByIdAndPersonId(Integer reportTemplateId, String personId) {
		ReportTemplate reportTemplate = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ReportTemplate> query = builder.createQuery(ReportTemplate.class);
		Root<ReportTemplate> rootReportTemplate = query.from(ReportTemplate.class);
		Predicate predicateOne = builder.equal(rootReportTemplate.get("reportTemplateId"), reportTemplateId);
		Predicate predicateTwo = builder.equal(rootReportTemplate.get("templateOwnerPersonId"), personId);
		Predicate predicateThree = builder.equal(rootReportTemplate.get(TEMPLATE_TYPE), "U");
		Predicate predicateFour = builder.equal(rootReportTemplate.get(TEMPLATE_TYPE), "S");
		Predicate predicateFive = builder.and(predicateOne, predicateTwo, predicateThree);
		Predicate predicateSix = builder.and(predicateOne, predicateFour);
		query.where(builder.or(predicateFive, predicateSix));
		try {
			reportTemplate = session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			return reportTemplate;
		}
		return reportTemplate;
	}

	@Override
	public List<ReportTemplate> fetchAllReportTemplatesBasedOnPersonId(String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ReportTemplate> query = builder.createQuery(ReportTemplate.class);
		Root<ReportTemplate> rootReportTemplate = query.from(ReportTemplate.class);
		Predicate predicateOne = builder.equal(rootReportTemplate.get("templateOwnerPersonId"), personId);
		Predicate predicateTwo = builder.equal(rootReportTemplate.get(TEMPLATE_TYPE), "S");
		Predicate predicateThree = builder.equal(rootReportTemplate.get(TEMPLATE_TYPE), "U");
		Predicate predicateFour = builder.and(predicateOne, predicateThree);
		Predicate predicateFive = builder.equal(rootReportTemplate.get("reportType").get("isActive"), "Y");
		Predicate predicateSix = builder.and(predicateTwo, predicateFive);
		query.where(builder.or(predicateFour, predicateSix));
		query.orderBy(builder.asc(rootReportTemplate.get("sortOrder")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ArrayList<HashMap<String, Object>> generateReportByQuery(String query) {
		ArrayList<HashMap<String, Object>> dataList = new ArrayList<>();
		try {
			dataList = dbEngine.executeQuerySQL(new ArrayList<>(), query);
		} catch (Exception e) {
			logger.error("exception in generateReportByQuery :{}", e.getMessage());
		}
		return dataList;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> downloadReportByQuery(String query) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			return session.createSQLQuery(query).list();
		}catch (Exception e) {
			return new ArrayList<>();
		}
		
	}

	@Override
	public ReportTemplate getReportTemplateById(Integer reportTemplateId) {
		return hibernateTemplate.get(ReportTemplate.class, reportTemplateId);
	}

	@Override
	public String getReportTemplateJsonByReportType(String reportTypeId) {
		String jsonName = "";
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT jsonName FROM ReportType WHERE typeCode = :reportTypeId";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("reportTypeId", reportTypeId);
		try {
			jsonName = (String) query.getSingleResult();
		} catch (Exception e) {
			return jsonName;
		}
		return jsonName;
	}

	@Override
	public String getJoinClause(String reportTypeCode) {
		String joinClause = "";
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT joinQuery FROM ReportTypeJoin WHERE typeCode = :reportTypeCode";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("reportTypeCode", reportTypeCode);
		try {
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return joinClause;
		}
	}

	@Scheduled(cron = "${report.datasync.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void sentAwardReportDataSync() {
		logger.info("Report Master Data set Sync Starts at : {}",commonDao.getCurrentTimestamp());
		if (syncCustomDataSet().get(0)!=null) {
			logger.info("Completed syncCustomDataSet : {}", commonDao.getCurrentTimestamp());
			if (syncAwardMasterDataSet().get(0)!=null) {
				logger.info("Completed syncAwardMasterDataSet : {}", commonDao.getCurrentTimestamp());
				if(syncDmpReportrDataSet().get(0)!=null) {
					logger.info("Completed syncDmpReportrDataSet : {}", commonDao.getCurrentTimestamp());
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	private List<Object[]> syncCustomDataSet() {
		logger.info("Report Master Data set Sync Custom Data Starts at : {}",commonDao.getCurrentTimestamp());
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		return session.createStoredProcedureCall("CUSTOM_DATA_SYNC").getResultList();
	}

	@SuppressWarnings("unchecked")
	private List<Object[]> syncAwardMasterDataSet() {
		logger.info("Report Master Data set Sync Award Master Stats at : {}",commonDao.getCurrentTimestamp());
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		return session.createStoredProcedureCall("AWARD_MASTER_DATASET_SYNC").getResultList();
	}

	@SuppressWarnings("unchecked")
	private List<Object[]> syncDmpReportrDataSet() {
		logger.info("Report Master Data set Sync DMP Stats at : {}",commonDao.getCurrentTimestamp());
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		return session.createStoredProcedureCall("DMP_REPORT_SYNC").getResultList();
	}

	@Override
	public String deleteReportTemplate(ReportTemplate reportTemplate) {
		try {
			hibernateTemplate.delete(reportTemplate);
		} catch (Exception e) {
			logger.error("Error occured in deleteReportTemplate : {}", e.getMessage());
			return "Error in deleting report template";
		}
		return "Report template deleted successfully";
	}

	@Override
	public List<ReportTemplate> fetchAllReportTemplates() {

		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ReportTemplate> query = builder.createQuery(ReportTemplate.class);
		Root<ReportTemplate> rootReportTemplate = query.from(ReportTemplate.class);
		Predicate predicateOne = builder.equal(rootReportTemplate.get(TEMPLATE_TYPE), "S");
		Predicate predicateTwo = builder.equal(rootReportTemplate.get(TEMPLATE_TYPE), "U");
		Predicate predicateThree = builder.equal(rootReportTemplate.get("reportType").get("isActive"), "Y");
		Predicate predicateFour = builder.and(predicateOne, predicateThree);
		query.where(builder.or(predicateFour, predicateTwo));
		query.orderBy(builder.asc(rootReportTemplate.get("sortOrder")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Object[]> administrativeDetails(CommonVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		List<Object[]> reportData = new ArrayList<>();
		try {
			CallableStatement statement = connection.prepareCall("{call GET_ADMINISTRATIVE_DETAILS(?)}");
			statement.setString(1, vo.getType());
			statement.execute();
			ResultSet resultSet = statement.getResultSet();
			while (resultSet.next()) {
				int colCount = resultSet.getMetaData().getColumnCount();
				Object[] objArray = new Object[colCount];
				for (int i = 0; i < colCount; i++) {
					objArray[i] = resultSet.getObject(i + 1);
				}
				reportData.add(objArray);
			}
		} catch (Exception e) {
			logger.info("Exception in administrative details {} ", e.getMessage());
		}
		return reportData;
	}

	@Override
	public List<ReportColumns> getReportColumnsByType(String reportTypeId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ReportColumns> query = builder.createQuery(ReportColumns.class);
		Root<ReportColumns> rootReportColumns = query.from(ReportColumns.class);
		Predicate predicateOne = builder.equal(rootReportColumns.get("typeCode"), reportTypeId);
		query.where(predicateOne);
		return session.createQuery(query).getResultList();
	}
}
