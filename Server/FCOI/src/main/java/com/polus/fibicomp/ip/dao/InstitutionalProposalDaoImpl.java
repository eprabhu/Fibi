package com.polus.fibicomp.ip.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
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

import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.ip.pojo.InstituteProposalActionLog;
import com.polus.fibicomp.ip.pojo.InstituteProposalActionType;
import com.polus.fibicomp.ip.pojo.InstituteProposalAdminDetail;
import com.polus.fibicomp.ip.pojo.InstituteProposalAttachType;
import com.polus.fibicomp.ip.pojo.InstituteProposalAttachments;
import com.polus.fibicomp.ip.pojo.InstituteProposalBudgetHeader;
import com.polus.fibicomp.ip.pojo.InstituteProposalComment;
import com.polus.fibicomp.ip.pojo.InstituteProposalKeywords;
import com.polus.fibicomp.ip.pojo.InstituteProposalPerson;
import com.polus.fibicomp.ip.pojo.InstituteProposalPersonAttachment;
import com.polus.fibicomp.ip.pojo.InstituteProposalPersonUnit;
import com.polus.fibicomp.ip.pojo.InstituteProposalResearchArea;
import com.polus.fibicomp.ip.pojo.InstituteProposalSpecialReview;
import com.polus.fibicomp.ip.pojo.InstituteProposalStatus;
import com.polus.fibicomp.ip.pojo.InstituteProposalType;
import com.polus.fibicomp.ip.pojo.InstituteProposalVersionHistory;
import com.polus.fibicomp.ip.vo.InstProposalVO;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.SpecialReviewType;
import com.polus.fibicomp.proposal.pojo.ProposalAttachment;
import com.polus.fibicomp.security.AuthenticatedUser;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "institutionalProposalDao")
public class InstitutionalProposalDaoImpl implements InstitutionalProposalDao {

	protected static Logger logger = LogManager.getLogger(InstitutionalProposalDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	private static final String UPDATE_TIMESTAMP = "updateTimeStamp";

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public boolean createInstitutionalProposal(Integer proposalId, String ipNumber, String userName) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			String functionName = "FN_FEED_INST_PROPOSAL";
			String functionCall = "{ ? = call " + functionName + "(?,?,?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setInt(2, proposalId);
			statement.setString(3, ipNumber);
			statement.setString(4, userName);
			statement.execute();
			int result = statement.getInt(1);
			if (result == 1) {
				return true;
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return false;
	}
	
	@Override
	public InstituteProposal fetchInstProposalById(Integer proposalId) {
		return hibernateTemplate.get(InstituteProposal.class, proposalId);
	}

	@Override
	public InstituteProposalAttachments fetchAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(InstituteProposalAttachments.class, attachmentId);
	}

	@Override
	public List<Integer> fetchDevProposalByInstProposal(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<InstituteProposalAdminDetail> proposalAdminDetail = query.from(InstituteProposalAdminDetail.class);
		query.where(builder.equal(proposalAdminDetail.get("instProposalId"), proposalId));
		query.orderBy(builder.desc(proposalAdminDetail.get("proposalAdminDetailId")));
		query.select(proposalAdminDetail.get("devProposalId"));
		query.distinct(true);
		return session.createQuery(query).getResultList();
	}

	@Override
	public InstituteProposalStatus fetchInstituteProposalStatusById(Integer statusCode) {
		return hibernateTemplate.get(InstituteProposalStatus.class, statusCode);
	}

	@Override
	public InstituteProposal saveOrUpdateInstituteProposal(InstituteProposal instituteProposal) {
		hibernateTemplate.saveOrUpdate(instituteProposal);
		return instituteProposal;
	}

	@Override
	public List<InstituteProposalStatus> fetchAllInstituteProposalStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalStatus> query = builder.createQuery(InstituteProposalStatus.class);
		Root<InstituteProposalStatus> rootUnit = query.from(InstituteProposalStatus.class);
		query.orderBy(builder.asc(rootUnit.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<InstituteProposalAttachments> getInstituteProposalAttachments(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalAttachments> query = builder.createQuery(InstituteProposalAttachments.class);
		Root<InstituteProposalAttachments> attachmentRoot = query.from(InstituteProposalAttachments.class);
		Predicate predicateProposalId = builder.equal(attachmentRoot.get("proposalId"), proposalId);
		query.where(builder.and(predicateProposalId));
		List <InstituteProposalAttachments> attachments = session.createQuery(query).getResultList();
		Set<String> userName = attachments.stream().map(InstituteProposalAttachments::getUpdateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			attachments.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setLastUpdateUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
		}
		return attachments;
	}

	@Override
	public List<InstituteProposalPerson> loadInstProposalPersonsByProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalPerson> query = builder.createQuery(InstituteProposalPerson.class);
		Root<InstituteProposalPerson> proposalPersonRoot = query.from(InstituteProposalPerson.class);
		query.where(builder.equal(proposalPersonRoot.get("proposalId"), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<InstituteProposalSpecialReview> loadInstProposalSpecialReviewByProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalSpecialReview> query = builder.createQuery(InstituteProposalSpecialReview.class);
		Root<InstituteProposalSpecialReview> specialReviewRoot = query.from(InstituteProposalSpecialReview.class);
		query.where(builder.equal(specialReviewRoot.get("proposalId"), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<InstituteProposalResearchArea> loadInstProposalResearchAreaByProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalResearchArea> query = builder.createQuery(InstituteProposalResearchArea.class);
		Root<InstituteProposalResearchArea> researchAreaRoot = query.from(InstituteProposalResearchArea.class);
		query.where(builder.equal(researchAreaRoot.get("proposalId"), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public InstituteProposalBudgetHeader loadInstPropBudgetDetailsByProposalId(Integer proposalId) {
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalBudgetHeader> query = builder.createQuery(InstituteProposalBudgetHeader.class);
		Root<InstituteProposalBudgetHeader>budgetRoot = query.from(InstituteProposalBudgetHeader.class);
		query.where(builder.equal(budgetRoot.get("proposalId"), proposalId));
		return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<ProposalAttachment> fetchProposalAttachmentsByProposalIds(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalAttachment> query = builder.createQuery(ProposalAttachment.class);
		Root<ProposalAttachment> rootProposalAttachment = query.from(ProposalAttachment.class);
		query.where(rootProposalAttachment.get("proposalId").in(proposalId));
		query.orderBy(builder.desc(rootProposalAttachment.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void saveOrUpdateInstituteProposalPerson(InstituteProposalPerson copyInstituteProposalPerson) {
		hibernateTemplate.saveOrUpdate(copyInstituteProposalPerson);
	}

	@Override
	public void saveOrUpdateIPResearchAreas(InstituteProposalResearchArea copyInstituteProposalResearchArea) {
		hibernateTemplate.saveOrUpdate(copyInstituteProposalResearchArea);
	}

	@Override
	public void saveOrUpdateIPBudgetDetails(InstituteProposalBudgetHeader copyInstituteProposalBudgetHeader) {
		hibernateTemplate.saveOrUpdate(copyInstituteProposalBudgetHeader);		
	}

	@Override
	public void saveOrUpdateSpecialReview(InstituteProposalSpecialReview copyIPSpecialReview) {
		hibernateTemplate.saveOrUpdate(copyIPSpecialReview);
	}

	@Override
	public List<InstituteProposalAdminDetail> loadAllProposalAdminDetails(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalAdminDetail> query = builder.createQuery(InstituteProposalAdminDetail.class);
		Root<InstituteProposalAdminDetail> rootIpAdminDetails = query.from(InstituteProposalAdminDetail.class);
		query.where(rootIpAdminDetails.get("instProposalId").in(proposalId));
		query.orderBy(builder.asc(rootIpAdminDetails.get("proposalAdminDetailId")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void saveOrUpdateIPAdminDetail(InstituteProposalAdminDetail copyIPAdminDetail) {
		hibernateTemplate.saveOrUpdate(copyIPAdminDetail);
	}

	@Override
	public void changeIPStatus(Integer statusCode, Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<InstituteProposal> criteriaUpdate = cb.createCriteriaUpdate(InstituteProposal.class);
		Root<InstituteProposal> root = criteriaUpdate.from(InstituteProposal.class);
		criteriaUpdate.set("statusCode", statusCode);		
		criteriaUpdate.where(cb.equal(root.get("proposalId"), proposalId));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public void deleteInstProposalPersonUnit(InstituteProposalPersonUnit proposalPersonUnit) {
		hibernateTemplate.delete(proposalPersonUnit);
	}

	@Override
	public void deleteIPKeyPerson(Integer keyPersonId) {
		hibernateTemplate.delete(hibernateTemplate.load(InstituteProposalPerson.class, keyPersonId));
	}

	@Override
	public void deleteIPPersonAttachment(Integer replaceAttachmentId) {
		hibernateTemplate.delete(hibernateTemplate.load(InstituteProposalPersonAttachment.class, replaceAttachmentId));	
	}

	@Override
	public void saveOrUpdateIPSpecialReview(InstituteProposalSpecialReview instituteProposalSpecialReview) {
		hibernateTemplate.saveOrUpdate(instituteProposalSpecialReview);
	}

	@Override
	public void saveOrUpdateIPAreaOfResearch(InstituteProposalResearchArea instituteProposalResearchArea) {
		hibernateTemplate.saveOrUpdate(instituteProposalResearchArea);
	}

	@Override
	public void saveOrUpdateIPBudget(InstituteProposalBudgetHeader instituteProposalBudgetHeader) {
		hibernateTemplate.saveOrUpdate(instituteProposalBudgetHeader);
	}

	@Override
	public void deleteIPSpecialReview(Integer specialReviewId) {
		hibernateTemplate.delete(hibernateTemplate.load(InstituteProposalSpecialReview.class, specialReviewId));		
	}

	@Override
	public void deleteIPAreaOfResearch(Integer areaOfResearchId) {
		hibernateTemplate.delete(hibernateTemplate.load(InstituteProposalResearchArea.class, areaOfResearchId));		
	}

	@Override
	public void deleteIPBudgetData(Integer budgetHeaderId) {
		hibernateTemplate.delete(hibernateTemplate.load(InstituteProposalBudgetHeader.class, budgetHeaderId));			
	}

	@Override
	public void saveOrUpdateIPAttachment(InstituteProposalAttachments instituteProposalAttachment) {
		hibernateTemplate.saveOrUpdate(instituteProposalAttachment);
		
	}

	@Override
	public void deleteIPAttachment(Integer attachmentId) {
		hibernateTemplate.delete(hibernateTemplate.load(InstituteProposalAttachments.class, attachmentId));		
	}

	@Override
	public InstituteProposalAttachments fetchIPAttachmentById(Integer attachmentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalAttachments> query = builder.createQuery(InstituteProposalAttachments.class);
		Root<InstituteProposalAttachments> attachmentRoot = query.from(InstituteProposalAttachments.class);
		Predicate predicateProposalId = builder.equal(attachmentRoot.get("attachmentId"), attachmentId);
		query.where(builder.and(predicateProposalId));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public void updateSequenceStatus(Integer proposalId, String sequenceStatus) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<InstituteProposal> criteriaUpdate = cb.createCriteriaUpdate(InstituteProposal.class);
		Root<InstituteProposal> instPropRoot = criteriaUpdate.from(InstituteProposal.class);
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("proposalSequenceStatus", sequenceStatus);
		criteriaUpdate.where(cb.equal(instPropRoot.get("proposalId"),proposalId)); 																			 
		session.createQuery(criteriaUpdate).executeUpdate();	
	}

	@Override
	public void saveIPVersionHistory(InstituteProposalVersionHistory history) {
		hibernateTemplate.saveOrUpdate(history);
	}

	@Override
	public void saveOrUpdateActionLog(InstituteProposalActionLog actionLog) {
		hibernateTemplate.saveOrUpdate(actionLog);
		
	}

	@Override
	public void saveOrUpdateIPKeyword(InstituteProposalKeywords instituteProposalKeyword) {
		hibernateTemplate.saveOrUpdate(instituteProposalKeyword);
	}

	@Override
	public List<InstituteProposalKeywords> fetchAllIPKeywordByProposal(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalKeywords> query = builder.createQuery(InstituteProposalKeywords.class);
		Root<InstituteProposalKeywords> ipKeyword = query.from(InstituteProposalKeywords.class);
		Predicate predicateProposalId = builder.equal(ipKeyword.get("instProposal").get("proposalId"), proposalId);
		query.where(builder.and(predicateProposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteIPKeyword(Integer keywordId) {
		hibernateTemplate.delete(hibernateTemplate.load(InstituteProposalKeywords.class, keywordId));				
	}

	@Override
	public void updateProposalTimestampAndUser(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<InstituteProposal> criteriaUpdate = cb.createCriteriaUpdate(InstituteProposal.class);
		Root<InstituteProposal> instPropRoot = criteriaUpdate.from(InstituteProposal.class);
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(instPropRoot.get("proposalId"),proposalId)); 																			 
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<SpecialReviewType> getSpecialReviewTypes(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT T1 FROM SpecialReviewType T1 inner join SpecialReviewUsage T2 on T1.specialReviewTypeCode = T2.specialReviewTypeCode WHERE T2.global=:global and T2.active=:active and T2.moduleCode=:moduleCode";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("global", Boolean.TRUE);
		query.setParameter("active", Boolean.TRUE);
		query.setParameter("moduleCode", moduleCode.toString());
		return query.getResultList();
	}

	@Override
	public Integer getNextSequenceNumber(String proposalNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "select max(sequenceNumber) FROM InstituteProposal where proposalNumber=:proposalNumber";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("proposalNumber", proposalNumber);
		return (Integer) query.getSingleResult();
	}

	@Override
	public List<InstituteProposalAttachType> fetchAllIPAttachmentTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();		
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalAttachType> query = builder.createQuery(InstituteProposalAttachType.class);
		Root<InstituteProposalAttachType> attachmentType = query.from(InstituteProposalAttachType.class);
		query.orderBy(builder.asc(attachmentType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<InstituteProposalType> fetchAllIPTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalType> query = builder.createQuery(InstituteProposalType.class);
		Root<InstituteProposalType> proposalType = query.from(InstituteProposalType.class);
		query.orderBy(builder.asc(proposalType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public InstituteProposalVersionHistory getIPVersionHistory(Integer originatedProposal) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalVersionHistory> query = builder.createQuery(InstituteProposalVersionHistory.class);
		Root<InstituteProposalVersionHistory> versionHistory = query.from(InstituteProposalVersionHistory.class);
		query.where(builder.equal(versionHistory.get("originatedProposalId"),originatedProposal)); 																			 
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public List<InstituteProposalActionType> fetchIPActionTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();		
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalActionType> query = builder.createQuery(InstituteProposalActionType.class);
		Root<InstituteProposalActionType> actionType = query.from(InstituteProposalActionType.class);
		query.where(builder.equal(actionType.get("isActive"),true)); 
		query.orderBy(builder.asc(actionType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<InstituteProposalActionLog> getIPActionHistory(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();		
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalActionLog> query = builder.createQuery(InstituteProposalActionLog.class);
		Root<InstituteProposalActionLog> actionLog = query.from(InstituteProposalActionLog.class);
		query.where(builder.equal(actionLog.get("proposalId"), proposalId)); 	
		query.orderBy(builder.desc(actionLog.get("updateTimeStamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void mergeProposalToIP(Integer proposalId, InstProposalVO vo) throws SQLException {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call MERGE_INSTITUTE_PROPOSAL (?,?,?,?,?,?)}");
				statement.setInt(1, vo.getDevProposalId());
				statement.setInt(2, proposalId);
				statement.setString(3, AuthenticatedUser.getLoginUserName());
				statement.setBoolean(4, vo.getIsKeyPersonMerge());
				statement.setBoolean(5, vo.getIsSpecialReviewMerge());
				statement.setBoolean(6, vo.getIsBudgetMerge());
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call MERGE_INSTITUTE_PROPOSAL (?,?,?,?,?,?,?)}");
				statement.setInt(1, vo.getDevProposalId());
				statement.setInt(2, proposalId);
				statement.setString(3, AuthenticatedUser.getLoginUserName());
				statement.setBoolean(4, vo.getIsKeyPersonMerge());
				statement.setBoolean(5, vo.getIsSpecialReviewMerge());
				statement.setBoolean(6, vo.getIsBudgetMerge());
				statement.registerOutParameter(7, OracleTypes.CURSOR);
				statement.execute();
			}	
	}

	@Override
	public Boolean anyAwardLinkedToIP(String proposalNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<Long> query = cb.createQuery(Long.class);
			Root<AwardFundingProposal> root = query.from(AwardFundingProposal.class);
			query.select(cb.count(root));
			query.where(cb.equal(root.get("proposal").get("proposalNumber"), proposalNumber));
			Long count = session.createQuery(query).getSingleResult();
			return count.intValue() > 0;
		} catch (Exception e) {
			return Boolean.FALSE;
		}
	}

	@Override
	public Boolean anyAwardInPendingSequence(String proposalNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<Long> query = cb.createQuery(Long.class);
			Root<InstituteProposal> root = query.from(InstituteProposal.class);
			query.select(cb.count(root));
			query.where(cb.equal(root.get("proposalNumber"), proposalNumber), cb.equal(root.get("proposalSequenceStatus"), "ACTIVE"));
			Long count = session.createQuery(query).getSingleResult();
			return count.intValue() > 0;
		} catch (Exception e) {
			return Boolean.FALSE;
		}
	}

	@Override
	public Integer getActiveInstProposalId(String proposalNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<Integer> query = cb.createQuery(Integer.class);
			Root<InstituteProposal> root = query.from(InstituteProposal.class);
			query.select(root.get("proposalId"));
			query.where(cb.equal(root.get("proposalNumber"), proposalNumber), cb.equal(root.get("proposalSequenceStatus"), "ACTIVE"));
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public Boolean checkForPIDifference(String proposalNumber, Integer devProposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder("select count(*) from ProposalPerson where (personId,rolodexId,personRoleId)not in ").
        		append("(select T1.personId,T1.rolodexId,T1.personRoleId from InstituteProposalPerson T1 ").
        		append("inner join InstituteProposal T2 on T1.proposalId = T2.proposalId and T2.proposalSequenceStatus = 'ACTIVE' where T1.personRoleId = 3 and T2.proposalNumber =:proposalNumber) and personRoleId = 3 and proposalId =:devProposalId ");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("devProposalId", devProposalId);
        query.setParameter("proposalNumber", proposalNumber);
        Integer count = Integer.parseInt(query.getSingleResult().toString());
        return count > 0;
	}

	@Override
	public String getIPTitleForMasterProposal(String baseProposalNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<String> query = cb.createQuery(String.class);
			Root<InstituteProposal> root = query.from(InstituteProposal.class);
			query.select(root.get("title"));
			query.where(cb.equal(root.get("proposalNumber"), baseProposalNumber), cb.equal(root.get("proposalSequenceStatus"), "ACTIVE"));
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<InstituteProposalComment> fetchInstituteProposalCommentsByParams(Integer instProposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalComment> query = builder.createQuery(InstituteProposalComment.class);
		Root<InstituteProposalComment> instituteProposalComment = query.from(InstituteProposalComment.class);
		query.where(builder.equal(instituteProposalComment.get("proposalId"), instProposalId));
		query.orderBy(builder.desc(instituteProposalComment.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public InstituteProposalComment saveOrUpdateInstituteProposalComment(InstituteProposalComment instituteProposalComment) {
		hibernateTemplate.saveOrUpdate(instituteProposalComment);
		return instituteProposalComment;
	}

	@Override
	public String getIPTitle(String baseProposalNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder("select title FROM InstituteProposal where proposalNumber=:proposalNumber and").
				append(" sequenceNumber IN (SELECT MAX(sequenceNumber) FROM InstituteProposal WHERE  proposalNumber=:proposalNumber)");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("proposalNumber", baseProposalNumber);
		return (String) query.getSingleResult();
	}

	@Override
	public void updateIPBudgetDates(Timestamp startDate, Timestamp endDate, Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<InstituteProposalBudgetHeader> criteriaUpdate = cb.createCriteriaUpdate(InstituteProposalBudgetHeader.class);
		Root<InstituteProposalBudgetHeader> rootBudgetHeader = criteriaUpdate.from(InstituteProposalBudgetHeader.class);
		Predicate predicateProposalId = cb.equal(rootBudgetHeader.get("proposalId"), proposalId);
		Predicate isLatestVersion = cb.equal(rootBudgetHeader.get("isLatestVersion"), Boolean.TRUE);
		criteriaUpdate.set("startDate", startDate);
		criteriaUpdate.set("endDate", endDate);
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.where(predicateProposalId, isLatestVersion);		 		
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> fetchIPHistoryDetails(String proposalNumber) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select T1.proposalId, T2.requestType, T1.sequenceNumber, T1.proposalSequenceStatus, T1.createUser, T1.createTimeStamp ");
		hqlQuery.append("from InstituteProposal T1 left join InstituteProposalVersionHistory T2 on T2.activeProposalId = T1.proposalId ");
		hqlQuery.append("where T1.proposalNumber =:proposalNumber order by T1.proposalId desc");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("proposalNumber", proposalNumber);
		return query.getResultList();
	}

	@Override
	public InstituteProposal getIPForMasterProposal(String baseProposalNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<InstituteProposal> query = cb.createQuery(InstituteProposal.class);
			Root<InstituteProposal> root = query.from(InstituteProposal.class);
			query.where(cb.equal(root.get("proposalNumber"), baseProposalNumber), cb.equal(root.get("proposalSequenceStatus"), "ACTIVE"));
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			logger.error("getIPForMasterProposal {}", e.getMessage());
			return null;
		}
	}
}
