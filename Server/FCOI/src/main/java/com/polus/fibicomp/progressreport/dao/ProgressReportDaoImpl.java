package com.polus.fibicomp.progressreport.dao;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.persistence.ParameterMode;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import com.polus.fibicomp.award.pojo.ReportClass;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardKPICriteria;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportAchievement;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportAttachment;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportKPISummary;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportMilestone;
import com.polus.fibicomp.progressreport.pojo.KPIManpowerDevelopmentCurrentStatus;
import com.polus.fibicomp.progressreport.pojo.KPIPublicationStatus;
import com.polus.fibicomp.progressreport.pojo.KPITechnologyDisclosureStatus;
import com.polus.fibicomp.progressreport.pojo.ProgressReportAchievementType;
import com.polus.fibicomp.progressreport.pojo.ProgressReportAttachmentType;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICashFunding;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICollaborationProjects;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICompetitiveGrants;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIConferencePresentation;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIGrantSpecific;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIHealthSpecificOutcomes;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIImpactPublications;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIInkindContributions;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPILicenses;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIManpowerDevelopment;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIMapping;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIPatents;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIPostDocsEmployed;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPISuccessfulStartups;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPITechnologiesDeployed;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPITechnologyDisclosure;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIUndergraduateStudent;
import com.polus.fibicomp.progressreport.pojo.ProgressReportStatus;
import com.polus.fibicomp.security.AuthenticatedUser;

@Service(value = "progressReportDao")
public class ProgressReportDaoImpl implements ProgressReportDao{

	protected static Logger logger = LogManager.getLogger(ProgressReportDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;
	
	@Autowired
	private CommonDao commonDao;

	@Override
	public void saveOrUpdateProgressReport(AwardProgressReport awardProgressReport) {
		hibernateTemplate.saveOrUpdate(awardProgressReport);		
	}

	@Override
	public AwardProgressReport loadAwardProgressReport(Integer progressReportId) {		
		return hibernateTemplate.get(AwardProgressReport.class, progressReportId);
	}

	@Override
	public List<String> getMileStoneNumbersNotInPR(List<String> mileStoneNumbers, Integer awardId) {
		try {
			return hibernateTemplate.execute(session -> {
				CriteriaBuilder builder = session.getCriteriaBuilder();
				CriteriaQuery<String> criteria = builder.createQuery(String.class);
				Root<AwardMileStone> milestoneRoot = criteria.from(AwardMileStone.class);				
				Predicate predicateAwardId = builder.equal(milestoneRoot.get("awardId"),awardId);
				Predicate predicateNotInMileStoneNumbers = builder.not(milestoneRoot.get("milestoneNumber").in(mileStoneNumbers));
				criteria.select(milestoneRoot.get("milestoneNumber"));
				criteria.where(builder.and(predicateAwardId, predicateNotInMileStoneNumbers));
				criteria.orderBy(builder.asc(milestoneRoot.get("updateTimeStamp")));
				return session.createQuery(criteria).getResultList();
			});	
		} catch (Exception e) {
			return new ArrayList<>();
		}
		
	}

	@Override
	public List<AwardProgressReportMilestone> loadProgressReportMilestone(Integer progressReportId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardProgressReportMilestone> criteria = builder.createQuery(AwardProgressReportMilestone.class);
			Root<AwardProgressReportMilestone> milestoneRoot = criteria.from(AwardProgressReportMilestone.class);				
			criteria.where(builder.equal(milestoneRoot.get("awardProgressReport").get("progressReportId"), progressReportId));
			criteria.orderBy(builder.desc(milestoneRoot.get("updateTimeStamp")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardMileStone> getAwardMilestoneByParam(Integer progressReportId) {		
		StringBuilder hqlQuery = new StringBuilder().append("select t1 from AwardMileStone t1 inner join AwardProgressReportMilestone t2 on t1.milestoneNumber = t2.milestoneNumber ");
		hqlQuery.append("where t2.awardProgressReport.progressReportId =:progressReportId and t1.awardMilestoneId in  ");
		hqlQuery.append("(select max(awardMilestoneId) from AwardMileStone where milestoneNumber = t2.milestoneNumber) group by t1.milestoneNumber ");
		Query queryMilestone = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryMilestone.setParameter("progressReportId",progressReportId);
		return queryMilestone.getResultList();	
	}

	@Override
	public void saveOrUpdateProgressReportMilestone(AwardProgressReportMilestone awardProgressReportMilestone) {
		hibernateTemplate.saveOrUpdate(awardProgressReportMilestone);	
	}

	@Override
	public String getProgressReportStatusCode(Integer progressReportId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<String> criteria = builder.createQuery(String.class);
			Root<AwardProgressReport> progressReport = criteria.from(AwardProgressReport.class);				
			criteria.select(progressReport.get("progressReportStatusCode"));
			criteria.where(builder.equal(progressReport.get("progressReportId"), progressReportId));
			return session.createQuery(criteria).getSingleResult();
		});
	}

	@Override
	public void updateProgressReportStatus(Integer progressReportId,
			String progressReportStatusCodeApprovalInProgress, Timestamp funderApprovalDate) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<AwardProgressReport> criteriaUpdate = cb.createCriteriaUpdate(AwardProgressReport.class);
		Root<AwardProgressReport> root = criteriaUpdate.from(AwardProgressReport.class);
		criteriaUpdate.set("progressReportStatusCode", progressReportStatusCodeApprovalInProgress);
		if (funderApprovalDate != null) {
			criteriaUpdate.set("funderApprovalDate", funderApprovalDate);
		}
		criteriaUpdate.where(cb.equal(root.get("progressReportId"), progressReportId));
		session.createQuery(criteriaUpdate).executeUpdate();	
	}

	@Override
	public ProgressReportStatus getProgressReportStatus(String claimStatusCodeApproved) {
		return hibernateTemplate.get(ProgressReportStatus.class, claimStatusCodeApproved);
	}

	@Override
	public String progressReportNextValue() {
		Query update = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery("update SEQ_AWARD_PROGRESS_REPORT_NUMBER set NEXT_VAL = NEXT_VAL+1");
		update.executeUpdate();
		Query select = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery("select CONCAT(lpad(NEXT_VAL,10,'0')) from SEQ_AWARD_PROGRESS_REPORT_NUMBER");
		return (String) select.getSingleResult();
	}

	@Override
	public void saveOrUpdateProgressReportAttachment(AwardProgressReportAttachment attachment) {
		hibernateTemplate.saveOrUpdate(attachment);		
	}

	@Override
	public Integer generateDocumentId() {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
			Root<AwardProgressReportAttachment> root = query.from(AwardProgressReportAttachment.class);
			query.select(builder.max(root.get("documentId")));
			Integer documentId =  session.createQuery(query).getSingleResult();
			return documentId == null ? 0 : documentId;
		}catch (Exception e) {
			return 0;
		}
	}

	@Override
	public void archiveOldAttachmentVersion(Integer documentId, Integer progressReportId, int versionNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<AwardProgressReportAttachment> criteriaUpdate = cb.createCriteriaUpdate(AwardProgressReportAttachment.class);
		Root<AwardProgressReportAttachment> root = criteriaUpdate.from(AwardProgressReportAttachment.class);
		criteriaUpdate.set("documentStatusCode", "2");
		Predicate predicateClaim = cb.equal(root.get("progressReportId"), progressReportId);
		Predicate predicateDocument = cb.equal(root.get("documentId"), documentId);
		Predicate predicateVersion = cb.equal(root.get("versionNumber") ,versionNumber);
		criteriaUpdate.where(cb.and(predicateClaim, predicateDocument, predicateVersion)); 		
		session.createQuery(criteriaUpdate).executeUpdate();		
	}

	@Override
	public void deleteProgressReportAttachment(AwardProgressReportAttachment awardProgressReportAttachment) {
		hibernateTemplate.delete(awardProgressReportAttachment);		
	}
	
	@Override
	public List<AwardProgressReportAttachment> loadProgressReportAttachments(Integer progressReportId, Boolean isPersonHasPermission) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardProgressReportAttachment> criteria = builder.createQuery(AwardProgressReportAttachment.class);
			Root<AwardProgressReportAttachment> root = criteria.from(AwardProgressReportAttachment.class);				
			Predicate predicateClaim = builder.equal(root.get("progressReportId"),progressReportId);
			Predicate predicateStatusCode = builder.equal(root.get("documentStatusCode"),"1");
			if (Boolean.TRUE.equals(isPersonHasPermission)) {
				criteria.where(builder.and(predicateClaim, predicateStatusCode));
			} else {
				Predicate predicateIsPrivate = builder.notEqual(root.get("progressReportAttachmentType").get("isPrivate"), true);
				criteria.where(builder.and(predicateClaim, predicateStatusCode, predicateIsPrivate));
			}
			criteria.orderBy(builder.desc(root.get("updateTimeStamp")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<AwardProgressReportAttachment> loadProgressReportAttachmentVersions(Integer progressReportId, Integer documentId, Boolean isPersonHasPermission) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardProgressReportAttachment> criteria = builder.createQuery(AwardProgressReportAttachment.class);
			Root<AwardProgressReportAttachment> root = criteria.from(AwardProgressReportAttachment.class);				
			Predicate predicateClaim = builder.equal(root.get("progressReportId"),progressReportId);
			Predicate predicateStatusCode = builder.equal(root.get("documentStatusCode"),"2");
			Predicate predicateDocument = builder.equal(root.get("documentId"),documentId);
			if (Boolean.TRUE.equals(isPersonHasPermission)) {
				criteria.where(builder.and(predicateClaim, predicateStatusCode, predicateDocument));
			} else {
				Predicate predicateIsPrivate = builder.notEqual(root.get("progressReportAttachmentType").get("isPrivate"), true);
				criteria.where(builder.and(predicateClaim, predicateStatusCode, predicateDocument, predicateIsPrivate));
			}
			criteria.orderBy(builder.desc(root.get("updateTimeStamp")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<ProgressReportAttachmentType> loadProgressReportAttachmentTypes() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ProgressReportAttachmentType> criteria = builder.createQuery(ProgressReportAttachmentType.class);
			Root<ProgressReportAttachmentType> attachmentType = criteria.from(ProgressReportAttachmentType.class);				
			criteria.where(builder.equal(attachmentType.get("isActive"), Boolean.TRUE));
			criteria.orderBy(builder.asc(attachmentType.get("description")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public AwardProgressReportAttachment getProgressReportAttachment(Integer attachmentId) {		
		return hibernateTemplate.get(AwardProgressReportAttachment.class, attachmentId);
	}

	@Override
	public List<ProgressReportAchievementType> loadProgressReportAchievementType() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ProgressReportAchievementType> criteria = builder.createQuery(ProgressReportAchievementType.class);
			Root<ProgressReportAchievementType> achievementType = criteria.from(ProgressReportAchievementType.class);				
			criteria.where(builder.equal(achievementType.get("isActive"), Boolean.TRUE));
			criteria.orderBy(builder.asc(achievementType.get("achievementTypeCode")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public void saveOrUpdateProgressReportAchievements(AwardProgressReportAchievement awardProgressReportAchievement) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<AwardProgressReportAchievement> criteriaUpdate = cb.createCriteriaUpdate(AwardProgressReportAchievement.class);
		Root<AwardProgressReportAchievement> root = criteriaUpdate.from(AwardProgressReportAchievement.class);
		criteriaUpdate.set("description",  awardProgressReportAchievement.getDescription());
		criteriaUpdate.where(cb.equal(root.get("progressReportAchievementId"), awardProgressReportAchievement.getProgressReportAchievementId())); 		
		session.createQuery(criteriaUpdate).executeUpdate();	
	}

	@Override
	public List<ProgressReportKPIMapping> getKPImapping() {		
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ProgressReportKPIMapping> criteria = builder.createQuery(ProgressReportKPIMapping.class);
			Root<ProgressReportKPIMapping> kpiMap = criteria.from(ProgressReportKPIMapping.class);				
			criteria.where(builder.equal(kpiMap.get("isActive"), Boolean.TRUE));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<AwardProgressReportKPISummary> loadProgressReportKPISummary(Integer progressReportId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardProgressReportKPISummary> criteria = builder.createQuery(AwardProgressReportKPISummary.class);
			Root<AwardProgressReportKPISummary> kpiSummary = criteria.from(AwardProgressReportKPISummary.class);				
			criteria.where(builder.equal(kpiSummary.get("awardProgressReport").get("progressReportId"), progressReportId));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<AwardKPICriteria> getAwardKPINotInPR(List<String> kpiCriteriaCode, Integer awardId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardKPICriteria> criteria = builder.createQuery(AwardKPICriteria.class);
			Root<AwardKPICriteria> kpiCriteria = criteria.from(AwardKPICriteria.class);				
			Predicate predicateAwardId = builder.equal(kpiCriteria.get("awardKPI").get("awardId"),awardId);
			Predicate predicateNotInMileStoneNumbers = builder.not(kpiCriteria.get("kpiCriteriaTypeCode").in(kpiCriteriaCode));
			criteria.where(builder.and(predicateAwardId, predicateNotInMileStoneNumbers));
			criteria.orderBy(builder.asc(kpiCriteria.get("updateTimeStamp")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<?> getSummaryDetail(String tableName, Integer kpiSummaryId){
		Class<?> cls = null;
		try {
			cls = Class.forName(tableName);
		} catch (ClassNotFoundException e) {
			logger.error("No class found",  e);
			return null;
		}
		StringBuilder hqlQuery = new StringBuilder().append("from ");
		hqlQuery.append(cls.getSimpleName()).append(" where kpiSummaryId =: kpiSummaryId");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiSummaryId",kpiSummaryId);
		return queryKPI.getResultList();
	}

	@Override
	public void saveOrUpdateKPISummaryDetail(Object summaryDetail) {
		hibernateTemplate.saveOrUpdate(summaryDetail);
		hibernateTemplate.flush();
	}

	@Override
	public void deleteKPISummaryDetail(Object entity) {
		hibernateTemplate.delete(entity);
	}

	@Override
	public List<KPIPublicationStatus> getKPIPublicationStatus() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<KPIPublicationStatus> criteria = builder.createQuery(KPIPublicationStatus.class);
			Root<KPIPublicationStatus> milestoneStatuses = criteria.from(KPIPublicationStatus.class);				
			criteria.where(builder.equal(milestoneStatuses.get("isActive"), Boolean.TRUE));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<KPITechnologyDisclosureStatus> getKPITechnologyDisclosureStatus() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<KPITechnologyDisclosureStatus> criteria = builder.createQuery(KPITechnologyDisclosureStatus.class);
			Root<KPITechnologyDisclosureStatus> milestoneStatuses = criteria.from(KPITechnologyDisclosureStatus.class);				
			criteria.where(builder.equal(milestoneStatuses.get("isActive"), Boolean.TRUE));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<AwardKPICriteria> getfetchAllAwardKPICriterias(Integer awardId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardKPICriteria> criteria = builder.createQuery(AwardKPICriteria.class);
			Root<AwardKPICriteria> milestoneStatuses = criteria.from(AwardKPICriteria.class);				
			criteria.where(builder.equal(milestoneStatuses.get("awardKPI").get("awardId"), awardId));
			return session.createQuery(criteria).getResultList();
		});
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getKPICriteriaAchievedValueHistory(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder().append("select distinct t2.progressReportNumber, t1.achieved,");
		hqlQuery.append(" t1.kpiSummaryId from AwardProgressReportKPISummary t1");
		hqlQuery.append(" inner join AwardProgressReport t2 on t2.progressReportId = t1.originatingProgressReportId");
		hqlQuery.append(" where t1.awardProgressReport.progressReportId =:progressReportId and t1.kpiCriteriaTypeCode =:kpiCriteriaTypeCode ");
	    Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		return queryKPI.getResultList();
	}

	@Override
	public List<String> getAllProgressReportNumberOfAward(String awardNumber) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<String> criteria = builder.createQuery(String.class);
			Root<AwardProgressReport> root = criteria.from(AwardProgressReport.class);				
			criteria.select(root.get("progressReportNumber"));
			criteria.where(builder.equal(root.get("awardNumber"), awardNumber));
			criteria.orderBy(builder.asc(root.get("progressReportId")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardProgressReport> loadProgressReportForAward(String awardNumber) {
		try{
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select distinct new com.polus.fibicomp.progressreport.pojo.AwardProgressReport (T2.awardReportTrackingId,T4.description AS reportClassDescription, T5.description AS reportTypeDescription, T1.progressReportId ,");
			hqlQuery.append(" T3.reportClassCode, T3.reportCode AS reportTypeCode,T1.awardId,T1.progressReportNumber,T1.awardNumber,T1.sequenceNumber,T1.progressReportStatusCode,");
			hqlQuery.append(" T6.description AS progressReportStatus,T1.dueDate,T1.createUser,T1.createTimeStamp,T1.updateUser,T1.updateTimeStamp, T1.reportStartDate, T1.reportEndDate) ");
			hqlQuery.append(" from AwardProgressReport T1 LEFT JOIN AwardReportTracking T2 ON T1.awardNumber = T2.awardNumber and T2.progressReportId = T1.progressReportId  LEFT JOIN AwardReportTerms T3  on T3.awardReportTermsId = T2.awardReportTerms.awardReportTermsId ");
			hqlQuery.append(" INNER JOIN ReportClass T4 ON T4.reportClassCode = T1.reportClassCode LEFT JOIN Report T5 ON T5.reportCode = T3.reportCode");
			hqlQuery.append(" INNER JOIN ProgressReportStatus T6 ON T6.progressReportStatusCode = T1.progressReportStatusCode");
			hqlQuery.append(" where T1.reportClassCode IN('1','2') AND T1.awardNumber = :awardNumber group by T1.progressReportId");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("awardNumber", awardNumber);
			return query.getResultList();
		} catch(Exception e) {
			logger.error("Error in loadProgressReportForAward {}", e.getMessage());
			return new ArrayList<>();
		}
	}

	@Override
	public List<KPIManpowerDevelopmentCurrentStatus> getKPIManpowerDevelopmentCurrentStatus() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<KPIManpowerDevelopmentCurrentStatus> criteria = builder.createQuery(KPIManpowerDevelopmentCurrentStatus.class);
			Root<KPIManpowerDevelopmentCurrentStatus> milestoneStatuses = criteria.from(KPIManpowerDevelopmentCurrentStatus.class);
			criteria.where(builder.equal(milestoneStatuses.get("isActive"), Boolean.TRUE));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
    public Timestamp getReportPeriodStartDate(AwardProgressReport awardProgressReport) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder hqlQuery = new StringBuilder();
			if(awardProgressReport.getReportClassCode().equals(Constants.FINAL_REPORT_CLASS_CODE)) {
				hqlQuery.append("select reportEndDate from AwardProgressReport p where p.awardNumber =:awardNumber and ");
				hqlQuery.append(" p.progressReportId in (select max(a.progressReportId) from AwardProgressReport a where a.progressReportId <>:progressReportId ");
				hqlQuery.append(" and a.awardNumber = p.awardNumber and a.progressReportId <:progressReportId and a.reportClassCode = '2' )");
			} else {
				hqlQuery.append("select reportEndDate from AwardProgressReport p where p.awardNumber =:awardNumber and ");
				hqlQuery.append(" p.progressReportId in (select max(a.progressReportId) from AwardProgressReport a where a.progressReportId <>:progressReportId ");
				hqlQuery.append(" and a.awardNumber = p.awardNumber and a.progressReportId <:progressReportId )");
			}
			Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("awardNumber", awardProgressReport.getAwardNumber());
			query.setParameter("progressReportId", awardProgressReport.getProgressReportId());
			return (Timestamp) query.getSingleResult();
		} catch (Exception e) {
			return null;
		}
    }

	@Override
	public List<AwardProgressReport> getAllProgressReport(List<Integer> trackingId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardProgressReport> criteria = builder.createQuery(AwardProgressReport.class);
			Root<AwardProgressReport> root = criteria.from(AwardProgressReport.class);
			criteria.multiselect(root.get("progressReportId"),root.get("progressReportNumber")
					,root.get("progressReportStatusCode"),root.get("dueDate")
					,root.get("createUser"),root.get("createTimeStamp")
					,root.get("progressReportStatus"));
			criteria.where(builder.in(root.get("awardReportTrackingId").in(trackingId)));
			criteria.orderBy(builder.desc(root.get("updateTimeStamp")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public void updateReportTracking(Integer progressReportId, Date dueDate, String awardNumber, String reportClassCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder().append("UPDATE AWARD_REPORT_TRACKING t1 "
				+ "INNER JOIN AWARD_REPORT_TERMS t2  on t1.AWARD_REPORT_TERMS_ID = t2.AWARD_REPORT_TERMS_ID and t2.AWARD_NUMBER =:awardNumber "
				+ "LEFT JOIN AWARD T3 on T2.AWARD_ID = T3.AWARD_ID and T3.AWARD_SEQUENCE_STATUS = 'PENDING' "
				+ "SET PROGRESS_REPORT_ID =:progressReportId WHERE T1.DUE_DATE =:dueDate AND T2.REPORT_CLASS_CODE =:reportClassCode");
		Query query = session.createSQLQuery(hqlQuery.toString());
		query.setParameter("awardNumber",awardNumber);
		query.setParameter("dueDate",dueDate);
		query.setParameter("progressReportId",progressReportId);
		query.setParameter("reportClassCode",reportClassCode);
		query.executeUpdate();
	}

	@Override
	public List<ProgressReportKPIImpactPublications> getProgressReportKPIImpactPublicationsBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPIImpactPublications> query = builder.createQuery(ProgressReportKPIImpactPublications.class);
		Root<ProgressReportKPIImpactPublications> impactPublication = query.from(ProgressReportKPIImpactPublications.class);				
		Predicate predicateKpiSummaryId = builder.equal(impactPublication.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPICollaborationProjects> getProgressReportKPICollaborationProjectsBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPICollaborationProjects> query = builder.createQuery(ProgressReportKPICollaborationProjects.class);
		Root<ProgressReportKPICollaborationProjects> collaborationProjects = query.from(ProgressReportKPICollaborationProjects.class);				
		Predicate predicateKpiSummaryId = builder.equal(collaborationProjects.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPITechnologyDisclosure> getProgressReportKPITechnologyDisclosureBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPITechnologyDisclosure> query = builder.createQuery(ProgressReportKPITechnologyDisclosure.class);
		Root<ProgressReportKPITechnologyDisclosure> technologyDisclosure = query.from(ProgressReportKPITechnologyDisclosure.class);				
		Predicate predicateKpiSummaryId = builder.equal(technologyDisclosure.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPIManpowerDevelopment> getProgressReportKPIManpowerDevelopmentBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPIManpowerDevelopment> query = builder.createQuery(ProgressReportKPIManpowerDevelopment.class);
		Root<ProgressReportKPIManpowerDevelopment> manpowerDevelopment = query.from(ProgressReportKPIManpowerDevelopment.class);				
		Predicate predicateKpiSummaryId = builder.equal(manpowerDevelopment.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPIUndergraduateStudent> getProgressReportKPIUndergraduateStudentBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPIUndergraduateStudent> query = builder.createQuery(ProgressReportKPIUndergraduateStudent.class);
		Root<ProgressReportKPIUndergraduateStudent> undergraduateStudent = query.from(ProgressReportKPIUndergraduateStudent.class);				
		Predicate predicateKpiSummaryId = builder.equal(undergraduateStudent.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPIConferencePresentation> getProgressReportKPIConferencePresentationBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPIConferencePresentation> query = builder.createQuery(ProgressReportKPIConferencePresentation.class);
		Root<ProgressReportKPIConferencePresentation> conferencePresentation = query.from(ProgressReportKPIConferencePresentation.class);				
		Predicate predicateKpiSummaryId = builder.equal(conferencePresentation.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPICompetitiveGrants> getProgressReportKPICompetitiveGrantsBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPICompetitiveGrants> query = builder.createQuery(ProgressReportKPICompetitiveGrants.class);
		Root<ProgressReportKPICompetitiveGrants> competitiveGrants = query.from(ProgressReportKPICompetitiveGrants.class);				
		Predicate predicateKpiSummaryId = builder.equal(competitiveGrants.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPIPatents> getProgressReportKPIPatentsBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPIPatents> query = builder.createQuery(ProgressReportKPIPatents.class);
		Root<ProgressReportKPIPatents> rootPatents = query.from(ProgressReportKPIPatents.class);				
		Predicate predicateKpiSummaryId = builder.equal(rootPatents.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPILicenses> getProgressReportKPILicensesBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPILicenses> query = builder.createQuery(ProgressReportKPILicenses.class);
		Root<ProgressReportKPILicenses> rootLicenses = query.from(ProgressReportKPILicenses.class);				
		Predicate predicateKpiSummaryId = builder.equal(rootLicenses.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPISuccessfulStartups> getProgressReportKPISuccessfulStartupsBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPISuccessfulStartups> query = builder.createQuery(ProgressReportKPISuccessfulStartups.class);
		Root<ProgressReportKPISuccessfulStartups> rootStartups = query.from(ProgressReportKPISuccessfulStartups.class);				
		Predicate predicateKpiSummaryId = builder.equal(rootStartups.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPIHealthSpecificOutcomes> getProgressReportKPIHealthSpecificOutcomesBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPIHealthSpecificOutcomes> query = builder.createQuery(ProgressReportKPIHealthSpecificOutcomes.class);
		Root<ProgressReportKPIHealthSpecificOutcomes> rootOutcomes = query.from(ProgressReportKPIHealthSpecificOutcomes.class);				
		Predicate predicateKpiSummaryId = builder.equal(rootOutcomes.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPIPostDocsEmployed> getProgressReportKPIPostDocsEmployedBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPIPostDocsEmployed> query = builder.createQuery(ProgressReportKPIPostDocsEmployed.class);
		Root<ProgressReportKPIPostDocsEmployed> rootEmployed = query.from(ProgressReportKPIPostDocsEmployed.class);				
		Predicate predicateKpiSummaryId = builder.equal(rootEmployed.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPIGrantSpecific> getProgressReportKPIGrantSpecificBasedOnCriteriaBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPIGrantSpecific> query = builder.createQuery(ProgressReportKPIGrantSpecific.class);
		Root<ProgressReportKPIGrantSpecific> rootGrant = query.from(ProgressReportKPIGrantSpecific.class);				
		Predicate predicateKpiSummaryId = builder.equal(rootGrant.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPICashFunding> getProgressReportKPICashFundingBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPICashFunding> query = builder.createQuery(ProgressReportKPICashFunding.class);
		Root<ProgressReportKPICashFunding> rootCashFunding = query.from(ProgressReportKPICashFunding.class);				
		Predicate predicateKpiSummaryId = builder.equal(rootCashFunding.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPIInkindContributions> getProgressReportKPIInkindContributionsBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPIInkindContributions> query = builder.createQuery(ProgressReportKPIInkindContributions.class);
		Root<ProgressReportKPIInkindContributions> rootInkind = query.from(ProgressReportKPIInkindContributions.class);				
		Predicate predicateKpiSummaryId = builder.equal(rootInkind.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProgressReportKPITechnologiesDeployed> getProgressReportKPITechnologiesDeployedBasedOnCriteria(Integer kpiSummaryId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession(); 
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProgressReportKPITechnologiesDeployed> query = builder.createQuery(ProgressReportKPITechnologiesDeployed.class);
		Root<ProgressReportKPITechnologiesDeployed> rootDeployed = query.from(ProgressReportKPITechnologiesDeployed.class);				
		Predicate predicateKpiSummaryId = builder.equal(rootDeployed.get("kpiSummaryId"), kpiSummaryId);
		query.where(builder.and(predicateKpiSummaryId));
		return session.createQuery(query).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Map<Object, Object>> getAllProgressReportNumberAndDueDateOfAward(Integer progressReportId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder().append("select distinct t.progressReportNumber, t.dueDate from AwardProgressReportKPISummary p ");
		hqlQuery.append("inner join AwardProgressReport t on t.progressReportId = p.originatingProgressReportId ");
		hqlQuery.append("where p.awardProgressReport.progressReportId =:progressReportId order by t.progressReportId asc ");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("progressReportId",progressReportId);
		List<Object[]> data = query.getResultList();
		List<Map<Object, Object>> summaryHistory = new ArrayList<>();
		Map<Object, Object> achievedValues = new HashMap<Object, Object>();
		data.forEach(summay -> {
			achievedValues.put("progressReportNumber", summay[0]);
			achievedValues.put("dueDate", summay[1]);
			summaryHistory.add(new HashMap<>(achievedValues));
			achievedValues.clear();
		});
		return summaryHistory;
	}

	@Override
	public AwardProgressReportAchievement checkProgressReportAchievementExist(String description, Integer progressReportId, Integer awardId) {
		try {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T2 From AwardProgressReport T1 inner join AwardProgressReportAchievement T2 on T2.awardProgressReport.progressReportId = T1.progressReportId");
		hqlQuery.append(" where T1.awardId = :awardId and T1.progressReportId = :progressReportId and T2.achievementTypeCode = (select achievementTypeCode from ");
		hqlQuery.append(" ProgressReportAchievementType where LTRIM(RTRIM(templateDescription)) = :description)");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("awardId", awardId);
		query.setParameter("progressReportId", progressReportId);
		query.setParameter("description", description);
		return (AwardProgressReportAchievement) query.getSingleResult();
		} catch(Exception e) {
			logger.error("error in checkProgressReportAchievementExist {}" , e.getMessage());
			return null;
		}
	}

	@Override
	public AwardProgressReportMilestone checkProgressReportMilestoneExist(Integer progressReportId, Integer awardId,
			Timestamp committedStartDate, Timestamp committedEndDate, String milestone) {
		try {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select T2 from AwardMileStone T1 inner join AwardProgressReportMilestone T2 on T2.milestoneNumber = T1.milestoneNumber");
		hqlQuery.append(" where T1.awardId = :awardId AND T2.awardProgressReport.progressReportId = :progressReportId AND T1.startDate <= :startDate AND T1.startDate >= :startDate AND T1.endDate <=:endDate AND T1.endDate >=:endDate AND LTRIM(RTRIM(T1.milestone)) = :milestone");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("awardId", awardId);
		query.setParameter("progressReportId", progressReportId);
		query.setParameter("startDate", committedStartDate);
		query.setParameter("endDate", committedEndDate);
		query.setParameter("milestone", milestone);
		return (AwardProgressReportMilestone) query.getSingleResult();
		} catch(Exception e) {
			logger.error("error in checkProgressReportMilestoneExist {}" , e.getMessage());
			return null;
		}
	}

	@Override
	public String getProgressMilestoneStatus(String description) {
		try {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select milestoneStatusCode From MilestoneStatus where description = :description");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("description", description);
		return (String) query.getSingleResult();
		} catch(Exception e) {
			logger.error("error in getProgressMilestoneStatus {}" ,e.getMessage());
			return null;
		}
	 }

	@Override
	public String getKpiCriteriaMappingDetail(String description) {
		try {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select distinct T2.sectionCode from KPICriteriaType T1 left join ProgressReportKPIMapping ");
		hqlQuery.append(" T2 on T2.kpiCriteriaTypeCode = T1.kpiCriteriaTypeCode where T1.description = :description");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("description", description);
		return (String) query.getSingleResult();
		} catch(Exception e) {
			logger.error("error in getKpiCriteriaMappingDetail {}" ,e.getMessage());
			return null;
		}
	}

	@Override
	public  AwardProgressReportKPISummary getKPISummaryDetailByParam(Integer awardId, Integer progressReportId, String description) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append(" from AwardProgressReportKPISummary where awardId =:awardId and awardProgressReport.progressReportId =:progressReportId and originatingProgressReportId = :progressReportId");
			hqlQuery.append(" and kpiCriteriaTypeCode = (select kpiCriteriaTypeCode from KPICriteriaType where description = :description)");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("awardId", awardId);
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("description", description);
			return (AwardProgressReportKPISummary) query.getSingleResult();
			} catch(Exception e) {
				logger.error("error in getKPISummaryDetailByParam {}", e.getMessage());
				return null;
			}
	}

	@Override
	public String getManpowerCurrentStatus(String description) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select currentStatusCode from KPIManpowerDevelopmentCurrentStatus where description =:description");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("description", description);
			Object value = query.getSingleResult();
			return value != null ? (String) value : null;
		} catch (Exception e) {
			logger.error("error in getManpowerCurrentStatus {}", e.getMessage());
			return null;
		}
	}

	@Override
	public String getsponsorCodeBySponsorName(String sponsorName) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select sponsorCode from Sponsor where sponsorName =:sponsorName");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("sponsorName", sponsorName);
			Object value = query.getSingleResult();
			return value != null ? (String) value : null ;
		} catch (Exception e) {
			logger.error("error in getsponsorCodeBySponsorName {}", e.getMessage());
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getAwardKpiCriterias(Integer awardId, Integer progressReportId) {
		try {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select distinct LTRIM(RTRIM(T1.description)) from KPICriteriaType T1 inner join AwardProgressReportKPISummary ");
		hqlQuery.append(" T2 on T2.kpiCriteriaTypeCode = T1.kpiCriteriaTypeCode where T2.awardId = :awardId and T2.awardProgressReport.progressReportId = :progressReportId");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("awardId", awardId);
		query.setParameter("progressReportId", progressReportId);
		return query.getResultList();
		} catch(Exception e) {
			logger.error("error in getAwardKpiCriterias {}", e.getMessage());
			return null;
		}
	}

	@Override
	public void deleteKPIImpactPublications(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiImpactPublicationId) {
		Query query = null;
		if (kpiImpactPublicationId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIImpactPublications c1 where c1.kpiImpactPublicationId =:kpiImpactPublicationId");
			query.setParameter("kpiImpactPublicationId", kpiImpactPublicationId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIImpactPublications c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteKpiCollaborationProjects(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiCollaborationProjectId) {
		Query query = null;
		if (kpiCollaborationProjectId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPICollaborationProjects c1 where c1.kpiCollaborationProjectId =:kpiCollaborationProjectId");
			query.setParameter("kpiCollaborationProjectId", kpiCollaborationProjectId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPICollaborationProjects c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteKpiTechnologyDisclosure(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiTechnologyDisclosureId) {
		Query query = null;
		if (kpiTechnologyDisclosureId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPITechnologyDisclosure c1 where c1.kpiTechnologyDisclosureId =:kpiTechnologyDisclosureId");
			query.setParameter("kpiTechnologyDisclosureId", kpiTechnologyDisclosureId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPITechnologyDisclosure c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteKPIUndergraduateStudent(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiUnderGraduateStudId) {
		Query query = null;
		if (kpiUnderGraduateStudId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIUndergraduateStudent c1 where c1.kpiUnderGraduateStudId =:kpiUnderGraduateStudId");
			query.setParameter("kpiUnderGraduateStudId", kpiUnderGraduateStudId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIUndergraduateStudent c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteKpiConferencePresentation(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiConferencePresentationId) {
		Query query = null;
		if(kpiConferencePresentationId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIConferencePresentation c1 where c1.kpiConferencePresentationId =:kpiConferencePresentationId");
			query.setParameter("kpiConferencePresentationId", kpiConferencePresentationId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIConferencePresentation c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteKpiCompetitiveGrants(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiCompetitiveGrantsId) {
		Query query = null;
		if(kpiCompetitiveGrantsId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPICompetitiveGrants c1 where c1.kpiCompetitiveGrantsId =:kpiCompetitiveGrantsId");
			query.setParameter("kpiCompetitiveGrantsId", kpiCompetitiveGrantsId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPICompetitiveGrants c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteProgressReportKPIPatents(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiPatentId) {
		Query query = null;
		if(kpiPatentId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIPatents c1 where c1.kpiPatentId =:kpiPatentId");
			query.setParameter("kpiPatentId", kpiPatentId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIPatents c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}


	@Override
	public void deleteProgressReportKpiLicenses(Integer progressReportId, String kpiCriteriaTypeCode,  Integer kpiLicenseId) {
		Query query = null;
		if (kpiLicenseId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPILicenses c1 where c1.kpiLicenseId =:kpiLicenseId");
			query.setParameter("kpiLicenseId", kpiLicenseId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPILicenses c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteProgressReportKPISuccessfulStartups(Integer progressReportId, String kpiCriteriaTypeCode,  Integer kpiSuccessfulStartupId) {
		Query query = null;
		if (kpiSuccessfulStartupId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPISuccessfulStartups c1 where c1.kpiSuccessfulStartupId =:kpiSuccessfulStartupId");
			query.setParameter("kpiSuccessfulStartupId", kpiSuccessfulStartupId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPISuccessfulStartups c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteProgressReportKPIHealthSpecificOutcomes(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiHealthSpecificOutcomeId) {
		Query query = null;
		if(kpiHealthSpecificOutcomeId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIHealthSpecificOutcomes c1 where c1.kpiHealthSpecificOutcomeId =:kpiHealthSpecificOutcomeId");
			query.setParameter("kpiHealthSpecificOutcomeId", kpiHealthSpecificOutcomeId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIHealthSpecificOutcomes c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteProgressReportKPIPostDocsEmployed(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiPostDocsEmployedId) {
		Query query = null;
		if (kpiPostDocsEmployedId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIPostDocsEmployed c1 where c1.kpiPostDocsEmployedId =:kpiPostDocsEmployedId");
			query.setParameter("kpiPostDocsEmployedId", kpiPostDocsEmployedId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIPostDocsEmployed c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteProgressReportKPIGrantSpecific(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiGrantSpecificId) {
		Query query = null;
		if (kpiGrantSpecificId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIGrantSpecific c1 where c1.kpiGrantSpecificId =:kpiGrantSpecificId");
			query.setParameter("kpiGrantSpecificId", kpiGrantSpecificId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIGrantSpecific c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteProgressReportKPICashFunding(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiCashFundingId) {
		Query query = null;
		if (kpiCashFundingId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPICashFunding c1 where c1.kpiCashFundingId =:kpiCashFundingId");
			query.setParameter("kpiCashFundingId", kpiCashFundingId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPICashFunding c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteProgressReportKPIInkindContributions(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiInkindContributionId) {
		Query query = null;
		if (kpiInkindContributionId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIInkindContributions c1 where c1.kpiInkindContributionId =:kpiInkindContributionId");
			query.setParameter("kpiInkindContributionId", kpiInkindContributionId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIInkindContributions c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public void deleteProgressReportKPITechnologiesDeployed(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiTechnologiesDeployedId) {
		Query query = null;
		if (kpiTechnologiesDeployedId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPITechnologiesDeployed c1 where c1.kpiTechnologiesDeployedId =:kpiTechnologiesDeployedId");
			query.setParameter("kpiTechnologiesDeployedId", kpiTechnologiesDeployedId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPITechnologiesDeployed c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getOrcidDataForProgressReportKPI(String awardNumber) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" SELECT t3.title,t3.JOURNAL_TITLE,t3.PUBLICATION_YEAR,t3.JOURNAL_TITLE as pulisher,GROUP_CONCAT( t4.cREDIT_NAME SEPARATOR ' , ') as AUTHORS,t3.ORCID_WORK_CATEGORY_CODE, t3.PUT_CODE ");
		hqlQuery.append(" from award_person_orcid_work t1 inner join person_orcid_work t2 on t1.PERSON_ORCID_WORK_ID = t2.PERSON_ORCID_WORK_ID ");
		hqlQuery.append(" inner join orcid_work t3 on t2.PUT_CODE = t3.PUT_CODE inner join orcid_work_contributor t4 on t3.PUT_CODE = t4.PUT_CODE ");
		hqlQuery.append(" where t1.AWARD_NUMBER =:awardNumber ");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		query.setParameter("awardNumber", awardNumber);
		return query.getResultList();
	}

	@Override
	public Date getReportPeriodStartDateByAwardNumber(String awardNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder hqlQuery = new StringBuilder();	
			hqlQuery.append("select reportEndDate from AwardProgressReport p where p.awardNumber =:awardNumber and ");
			hqlQuery.append(" p.progressReportId in (select max(a.progressReportId) from AwardProgressReport a where a.awardNumber = p.awardNumber )");
			Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("awardNumber", awardNumber);
			return (Timestamp) query.getSingleResult();
		} catch (Exception e) {
			return null;
		}			
	}

	@Override
	public Boolean checkInprogressProgressReportExist(String awardNumber) {
		try {
			return hibernateTemplate.execute(session -> {
				CriteriaBuilder builder = session.getCriteriaBuilder();
				CriteriaQuery<AwardProgressReport> criteria = builder.createQuery(AwardProgressReport.class);
				Root<AwardProgressReport> root = criteria.from(AwardProgressReport.class);				
				Predicate predicateClaim = builder.equal(root.get("awardNumber"),awardNumber);
				Predicate predicateStatusCode = builder.notEqual(root.get("progressReportStatusCode"),"4");
				criteria.where(builder.and(predicateClaim, predicateStatusCode));
				return session.createQuery(criteria).getResultList().isEmpty() ? false : true; 
			});
		} catch (Exception e) {
			return false;
		}
		
	}

	@Override
	public AwardProgressReport getLatestApprovedProgessReport(String awardNumber, String reportClassCode) {
		try {
			if(reportClassCode.equals(Constants.PROGRESS_REPORT_CLASS_CODE)) {
				StringBuilder hqlQueryAward = new StringBuilder();
				hqlQueryAward.append("from AwardProgressReport t1 where t1.awardNumber =:awardNumber and t1.progressReportStatusCode = '4'  and t1.reportClassCode = '1' and t1.progressReportId in  ");
				hqlQueryAward.append(" (select max(t2.progressReportId) from AwardProgressReport t2 where t2.awardNumber = t1.awardNumber and t2.progressReportStatusCode = t1.progressReportStatusCode and t2.reportClassCode = t1.reportClassCode) ");
				Query queryAward = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQueryAward.toString());
				queryAward.setParameter("awardNumber", awardNumber);
				return (AwardProgressReport) queryAward.getSingleResult();
			} else {
				StringBuilder hqlQueryAward = new StringBuilder();
				hqlQueryAward.append("from AwardProgressReport t1 where t1.awardNumber =:awardNumber and t1.progressReportStatusCode = '4'  and t1.progressReportId in  ");
				hqlQueryAward.append(" (select max(t2.progressReportId) from AwardProgressReport t2 where t2.awardNumber = t1.awardNumber and t2.progressReportStatusCode = t1.progressReportStatusCode) ");
				Query queryAward = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQueryAward.toString());
				queryAward.setParameter("awardNumber", awardNumber);
				return (AwardProgressReport) queryAward.getSingleResult();
			}			
		} catch (Exception e) {
			return null;
		}
		
	}

	@Override
	public void updateProgressReportUpdatedTimeAndUser(Integer progressReportId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<AwardProgressReport> criteriaUpdate = cb.createCriteriaUpdate(AwardProgressReport.class);
		Root<AwardProgressReport> root = criteriaUpdate.from(AwardProgressReport.class);
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.where(cb.equal(root.get("progressReportId"), progressReportId));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> fetchAwardProgressReportAchievementTypesById(Integer progressReportId, String sectionType) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" select LTRIM(RTRIM(T2.templateDescription)) From AwardProgressReportAchievement T1 inner join ProgressReportAchievementType T2 ON T1.achievementTypeCode = T2.achievementTypeCode");
		hqlQuery.append(" where T1.awardProgressReport.progressReportId = :progressReportId and T2.sectionType =:sectionType");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("progressReportId", progressReportId);
		query.setParameter("sectionType", sectionType);
		return query.getResultList();
	}

	@Override
	public void deleteKpiProgressReportKPIManpowerDevelopment(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiManpowerDevelopmentId) {
		Query query = null;
		if(kpiManpowerDevelopmentId != null) {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIManpowerDevelopment c1 where c1.kpiManpowerDevelopmentId =:kpiManpowerDevelopmentId");
			query.setParameter("kpiManpowerDevelopmentId", kpiManpowerDevelopmentId);
		} else {
			query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(" delete from ProgressReportKPIManpowerDevelopment c1 where c1.progressReportId =:progressReportId and c1.kpiCriteriaCode = :kpiCriteriaCode");
			query.setParameter("progressReportId", progressReportId);
			query.setParameter("kpiCriteriaCode", kpiCriteriaTypeCode);
		}
		query.executeUpdate();
	}

	@Override
	public String getAwardLeadUnit(Integer awardId) {
		return (String) hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<String> criteria = builder.createQuery(String.class);
			Root<Award> root = criteria.from(Award.class);				
			criteria.select(root.get("leadUnitNumber"));
			criteria.where(builder.equal(root.get("awardId"),awardId));
			return session.createQuery(criteria).getSingleResult();
		});	
	}

	@Override
	public String getReportClassCodeByProgressReportId(Integer progressReportId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String sumOfPayrollAmount = "SELECT reportClassCode FROM  AwardProgressReport where progressReportId =:progressReportId";
		Query query = session.createQuery(sumOfPayrollAmount);
		query.setParameter("progressReportId", progressReportId);
		return (String) query.getSingleResult();
	}

	@Override
	public Integer getDataTypeLengthForDescription(String field, String tableName) {
		Integer count = 0;
		try {
			StringBuilder hqlQuery = new StringBuilder().append("select distinct max(character_maximum_length) from information_schema.columns where table_name = :tableName and column_name =:field");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createNativeQuery(hqlQuery.toString());
			query.setParameter("tableName", tableName);
			query.setParameter("field", field);
			count = ((Number) query.getSingleResult()).intValue();
			return count;
		} catch (Exception e) {
			logger.error("Exception in getDataTypeLengthForDescription {}", e.getMessage());
			return count;
		}
	}

	@Override
	public Timestamp getAwardStartDateByAwardNumber(String awardNumber) {
		StringBuilder hqlQueryAward = new StringBuilder();
		hqlQueryAward.append(" select t1.beginDate from Award t1 where t1.awardNumber =:awardNumber and t1.awardSequenceStatus = 'ACTIVE' ");
		Query queryAward = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQueryAward.toString());
		queryAward.setParameter("awardNumber", awardNumber);
		return (Timestamp) queryAward.getSingleResult();
	}

	@Override
	public List<Integer> getPutCodeForOrcidInImpactPublications(Integer kpiSummaryId, String kpiCriteriaCode) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> criteria = builder.createQuery(Integer.class);
			Root<ProgressReportKPIImpactPublications> kpiSummaryRoot = criteria.from(ProgressReportKPIImpactPublications.class);	
			criteria.select(kpiSummaryRoot.get("putCode"));
			Predicate predicatekpiSummaryId = builder.equal(kpiSummaryRoot.get("kpiSummaryId"), kpiSummaryId);
			Predicate predicateKpiCriteriaTypeCode = builder.equal(kpiSummaryRoot.get("kpiCriteriaCode"), kpiCriteriaCode);
			Predicate predicateKpiPutCode = builder.isNotNull(kpiSummaryRoot.get("putCode"));
			criteria.where(builder.and(predicatekpiSummaryId, predicateKpiCriteriaTypeCode, predicateKpiPutCode));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public void updateKPIAchievedCountImpactPublication(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from (select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_impact_publications  t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.publication_date between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode  ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountCollaborationProjects(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from (select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_collaboration_projects  t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.project_start_date between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountTechnologyDisclosure(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from (select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_technology_disclosure  t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date_of_filing between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountManpowerDevelopment(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from (select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_manpower_development  t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date_of_joining between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountUndergraduateStudent(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from (select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_undergraduate_student  t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date_of_joining between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountConferencePresentation(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from (select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_conference_presentation t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountCompetitiveGrants(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from (select sum(t5.direct_cost) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_competitive_grants t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.project_start_date between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountPatents(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from(select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_patents t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date_granted between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountLicenses(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from(select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_licenses t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.start_date between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountSuccessfulStartups(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from(select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_successful_startups t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date_of_establishment between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountHealthSpecificOutcomes(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from(select sum(t5.number_of_life_years) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_health_specific_outcomes t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date_established between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountPostDocsEmployed(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from(select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_post_docs_employed t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.employment_start_date between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountPostGrantSpecific(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from(select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_grant_specific t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountCashFunding(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from(select sum(t5.amount) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_cash_funding t5 on t4.kpi_Summary_Id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date_of_contribution between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountInkindContributions(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from(select sum(t5.amount) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_inkind_contributions t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date_of_contribution between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public void updateKPIAchievedCountTechnologiesDeployed(String kpiCriteriaTypeCode, Integer progressReportId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append(" update award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join award_progress_report t2 on t2.award_number = t1.award_number ");
		hqlQuery.append(" inner join(select * from(select count(t5.progress_report_id) as amount, t6.progress_report_id from award_progress_report_kpi_summary t4 ");
		hqlQuery.append(" left join progress_report_kpi_technologies_deployed t5 on t4.kpi_summary_id = t5.kpi_summary_id ");
		hqlQuery.append(" inner join award_progress_report t6 on t4.award_number = t6.award_number ");
		hqlQuery.append(" where t5.date_of_deploying between t6.report_start_date and t6.report_end_date and t4.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		hqlQuery.append(" and t4.progress_report_id =:progressReportId group by t6.progress_report_id  ");
		hqlQuery.append(" union all select distinct null as amount,t2.progress_report_id from award_progress_report_kpi_summary t1 ");
		hqlQuery.append(" inner join  award_progress_report t2 on t1.award_number = t2.award_number where t1.progress_report_id =:progressReportId)s group by  s.progress_report_id ");
		hqlQuery.append(" )x on x.progress_report_id = t1.orginating_progress_report_id ");
		hqlQuery.append(" set t1.achieved = x.amount where t1.progress_report_id =:progressReportId and t1.kpi_criteria_type_code =:kpiCriteriaTypeCode ");
		Query queryKPI = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
		queryKPI.setParameter("kpiCriteriaTypeCode", kpiCriteriaTypeCode);
		queryKPI.setParameter("progressReportId", progressReportId);
		queryKPI.executeUpdate();
	}

	@Override
	public Object getProgressReportKPIDetailById(String tableName, Integer id) {
		tableName = "com.polus.fibicomp.progressreport.pojo." +tableName;
		Class<?> cls;
		try {
			cls = Class.forName(tableName);
		} catch (ClassNotFoundException e) {
			logger.error("No class found", e);
			return null;
		}
		return hibernateTemplate.get(cls,id);
	}

	@Override
	public void updateProgressReportDates(Integer progressReportId, Timestamp reportStartDate, Timestamp reportEndDate, String title) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<AwardProgressReport> criteriaUpdate = cb.createCriteriaUpdate(AwardProgressReport.class);
		Root<AwardProgressReport> root = criteriaUpdate.from(AwardProgressReport.class);
		criteriaUpdate.set("reportStartDate", reportStartDate);
		criteriaUpdate.set("reportEndDate", reportEndDate);
		criteriaUpdate.set("title", title);
		criteriaUpdate.where(cb.equal(root.get("progressReportId"), progressReportId));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public List<AwardProgressReportKPISummary> getProgressReportKPISummaryById(Integer progressReportId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardProgressReportKPISummary> criteria = builder.createQuery(AwardProgressReportKPISummary.class);
			Root<AwardProgressReportKPISummary> kpiSummaryRoot = criteria.from(AwardProgressReportKPISummary.class);
			Predicate predicateProgressReportId = builder.equal(kpiSummaryRoot.get("awardProgressReport").get("progressReportId"), progressReportId);
			Predicate predicateOrgProgressReportId = builder.equal(kpiSummaryRoot.get("originatingProgressReportId"), progressReportId);
			criteria.where(builder.and(predicateProgressReportId,predicateOrgProgressReportId ));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public void deleteAwardProgressReport(Integer progressReportId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardProgressReport> delete = builder.createCriteriaDelete(AwardProgressReport.class);
		Root<AwardProgressReport> root = delete.from(AwardProgressReport.class);
		delete.where(builder.equal(root.get("progressReportId"), progressReportId));
		session.createQuery(delete).executeUpdate();		
	}

	@Override
	public void deleteAwardProgressReportKPISummary(Integer progressReportId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardProgressReportKPISummary> delete = builder.createCriteriaDelete(AwardProgressReportKPISummary.class);
		Root<AwardProgressReportKPISummary> root = delete.from(AwardProgressReportKPISummary.class);
		delete.where(builder.equal(root.get("awardProgressReport").get("progressReportId"), progressReportId));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public void deleteAwardProgressReportMilestone(Integer progressReportId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardProgressReportMilestone> delete = builder.createCriteriaDelete(AwardProgressReportMilestone.class);
		Root<AwardProgressReportMilestone> root = delete.from(AwardProgressReportMilestone.class);
		delete.where(builder.equal(root.get("awardProgressReport").get("progressReportId"), progressReportId));
		session.createQuery(delete).executeUpdate();		
	}

	@Override
	public void deleteAwardProgressReportAchievement(Integer progressReportId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardProgressReportAchievement> delete = builder.createCriteriaDelete(AwardProgressReportAchievement.class);
		Root<AwardProgressReportAchievement> root = delete.from(AwardProgressReportAchievement.class);
		delete.where(builder.equal(root.get("awardProgressReport").get("progressReportId"), progressReportId));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public void deleteAwardProgressReportAttachment(Integer progressReportId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardProgressReportAttachment> delete = builder.createCriteriaDelete(AwardProgressReportAttachment.class);
		Root<AwardProgressReportAttachment> root = delete.from(AwardProgressReportAttachment.class);
		delete.where(builder.equal(root.get("progressReportId"), progressReportId));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public void removeAwardReportTrackingProgressReportId(Integer progressReportId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query query = session.createSQLQuery("UPDATE award_report_tracking t1 SET t1.PROGRESS_REPORT_ID = null WHERE t1.PROGRESS_REPORT_ID =:progressReportId");
			query.setParameter("progressReportId", progressReportId);
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("Exception while update AwardReportTracking by ProgressReportId: {} and error Msg: {}", progressReportId , e);
		}
	}

	@Override
	public List<AwardProgressReportKPISummary> getProgressReportKPISummaryDetailsById(Integer progressReportId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardProgressReportKPISummary> criteria = builder.createQuery(AwardProgressReportKPISummary.class);
			Root<AwardProgressReportKPISummary> kpiSummaryRoot = criteria.from(AwardProgressReportKPISummary.class);
			Predicate predicateProgressReportId = builder.equal(kpiSummaryRoot.get("awardProgressReport").get("progressReportId"), progressReportId);
			criteria.where(builder.and(predicateProgressReportId));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public Object[] getProgressReportReportingPeriod(Integer awardId, Date dueDate, String reportClassCode) {	
		try {
			return (Object[]) hibernateTemplate.execute(session -> 
					session.createStoredProcedureCall("GET_PR_REPORTING_DATES").
					registerStoredProcedureParameter("AV_AWARD_ID", Integer.class, ParameterMode.IN)
					.registerStoredProcedureParameter("AV_DUE_DATE", Date.class, ParameterMode.IN)
					.registerStoredProcedureParameter("AV_REPORT_CLASS_CODE", String.class, ParameterMode.IN)
					.setParameter("AV_AWARD_ID", awardId)
					.setParameter("AV_DUE_DATE", dueDate)
					.setParameter("AV_REPORT_CLASS_CODE", reportClassCode)
					.getSingleResult()
			);	
		} catch (Exception e) {
			logger.info("No reporting dates while getting reporting period award id {} ", awardId);
		}
		return null;	
	}

	@Override
	public ReportClass getReportClass(String reportClassCode) {
		return hibernateTemplate.get(ReportClass.class, reportClassCode);
	}

	@Override
	public void updateProgressReportSubmitUser(Integer progressReportId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<AwardProgressReport> criteriaUpdate = cb.createCriteriaUpdate(AwardProgressReport.class);
		Root<AwardProgressReport> root = criteriaUpdate.from(AwardProgressReport.class);
		criteriaUpdate.set("submitUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("submissionDate", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(root.get("progressReportId"), progressReportId));
		session.createQuery(criteriaUpdate).executeUpdate();	
	}
}
