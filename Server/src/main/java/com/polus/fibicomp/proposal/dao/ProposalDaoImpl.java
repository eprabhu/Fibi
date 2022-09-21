package com.polus.fibicomp.proposal.dao;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;

import javax.persistence.LockModeType;
import javax.persistence.NoResultException;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
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
import org.springframework.data.jpa.repository.Lock;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.FibiProposalRate;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.compilance.pojo.ProposalSpecialReview;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanel;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.PersonRoleRT;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.proposal.pojo.CongressionalDistrict;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalAttachment;
import com.polus.fibicomp.proposal.pojo.ProposalComment;
import com.polus.fibicomp.proposal.pojo.ProposalCommentAttachment;
import com.polus.fibicomp.proposal.pojo.ProposalCongDistrict;
import com.polus.fibicomp.proposal.pojo.ProposalDocumentStatus;
import com.polus.fibicomp.proposal.pojo.ProposalEvaluationScore;
import com.polus.fibicomp.proposal.pojo.ProposalExtension;
import com.polus.fibicomp.proposal.pojo.ProposalHistory;
import com.polus.fibicomp.proposal.pojo.ProposalKPI;
import com.polus.fibicomp.proposal.pojo.ProposalKPICriteria;
import com.polus.fibicomp.proposal.pojo.ProposalKeyword;
import com.polus.fibicomp.proposal.pojo.ProposalMileStone;
import com.polus.fibicomp.proposal.pojo.ProposalOrganization;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.proposal.pojo.ProposalPersonAttachment;
import com.polus.fibicomp.proposal.pojo.ProposalPersonRoles;
import com.polus.fibicomp.proposal.pojo.ProposalProjectTeam;
import com.polus.fibicomp.proposal.pojo.ProposalStatus;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerAttachment;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerComment;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerScore;
import com.polus.fibicomp.security.AuthenticatedUser;
import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "proposalDao")
public class ProposalDaoImpl implements ProposalDao {

	protected static Logger logger = LogManager.getLogger(ProposalDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private CommonDao commonDao;

	@Value("${oracledb}")
	private String oracledb;

	private static final String FILE_NAME = "fileName";
	private static final String UPDATE_USER = "updateUser";
	private static final String PROPOSAL_ID = "proposalId";
	private static final String DESCRIPTION = "description";
	private static final String UPDATE_TIMESTAMP = "updateTimeStamp";
	private static final String STATUS_CODE = "statusCode";
	private static final String GRANT_CALL_ID = "grantCallId";

	@Lock(LockModeType.PESSIMISTIC_WRITE)
	@Override
	public Proposal saveOrUpdateProposal(Proposal proposal) {
		try {
			hibernateTemplate.saveOrUpdate(proposal);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposal : {} ", e.getMessage());
		}
		return proposal;
	}

	@Override
	public Proposal fetchProposalById(Integer proposalId) {
		return hibernateTemplate.get(Proposal.class, proposalId);
	}

	@Override
	public ProposalSpecialReview deleteProposalSpecialReview(ProposalSpecialReview specialReview) {
		hibernateTemplate.delete(specialReview);
		return specialReview;
	}

	@Override
	public String deleteProposal(Integer proposalId) {
		Proposal proposal = hibernateTemplate.get(Proposal.class, proposalId);
		hibernateTemplate.delete(proposal);
		return "Proposal deleted successfully";
	}
	
	@Override
	public String deleteManitainProjectTeam(Integer proposalProjectTeamId) {
		ProposalProjectTeam projectTeam = hibernateTemplate.get(ProposalProjectTeam.class, proposalProjectTeamId);
		hibernateTemplate.delete(projectTeam);
		return "Proposal project team deleted successfully";
	}

	@Override
	public String saveManitainProjectTeam(ProposalProjectTeam projectTeam) {
		hibernateTemplate.save(projectTeam);
		return "Proposal project team saved successfully";
	}

	@Override
	public String updateManitainProjectTeam(ProposalProjectTeam projectTeam) {
		hibernateTemplate.update(projectTeam);
		return "Proposal project team updated successfully";
	}

	@Override
	public ProposalPersonRoles saveProposalPersonRole(ProposalPersonRoles personRole) {
		hibernateTemplate.saveOrUpdate(personRole);
		return personRole;
	}

	@Override
	public String deleteProposalPersonRole(ProposalPersonRoles personRole) {
		hibernateTemplate.delete(personRole);
		return "Proposal Person Role deleted successfully";
	}

	@Override
	public String getMaxScienceKeyword() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String queryForMax = "SELECT MAX(CAST(SCIENCE_KEYWORD_CODE AS UNSIGNED INTEGER))+1 as VALUE FROM SCIENCE_KEYWORD";
		Query query = session.createSQLQuery(queryForMax);
		BigInteger maxValue = (BigInteger) query.getResultList().get(0);
		return maxValue + "";
	}

	@Override
	public ScienceKeyword saveOrUpdateScienceKeyword(ScienceKeyword scienceKeyword) {
		try {
			hibernateTemplate.saveOrUpdate(scienceKeyword);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateScienceKeyword: {} ", e.getMessage());
		}
		return scienceKeyword;
	}

	@Override
	public ProposalAttachment fetchProposalAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(ProposalAttachment.class, attachmentId);
	}

	@Override
	public List<ProposalAttachment> fetchSortedAttachments(Integer proposalId, String sortBy, String reverse) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalAttachment> query = builder.createQuery(ProposalAttachment.class);
		Root<ProposalAttachment> attachmentRoot = query.from(ProposalAttachment.class);
		Predicate predicateProposalId = builder.equal(attachmentRoot.get(PROPOSAL_ID), proposalId);
		query.where(builder.and(predicateProposalId));
		if (sortBy.equals("attachmentTypeCode")) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get("attachmentType").get(DESCRIPTION)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get("attachmentType").get(DESCRIPTION)));
			}
		} else if (sortBy.equals(DESCRIPTION)) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get(DESCRIPTION)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get(DESCRIPTION)));
			}
		} else if (sortBy.equals(FILE_NAME)) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get(FILE_NAME)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get(FILE_NAME)));
			}
		} else if (sortBy.equals(UPDATE_USER)) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get(UPDATE_USER)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get(UPDATE_USER)));
			}
		} else if (sortBy.equals(UPDATE_TIMESTAMP)) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get(UPDATE_TIMESTAMP)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get(UPDATE_TIMESTAMP)));
			}
		} else if (sortBy.equals("narrativeStatusCode")) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get("narrativeStatus").get(DESCRIPTION)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get("narrativeStatus").get(DESCRIPTION)));
			}
		}
		List <ProposalAttachment> attachments = session.createQuery(query).getResultList();
		for (ProposalAttachment attachment : attachments) {
			if (attachment.getUpdateUser() != null) {
				attachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(attachment.getUpdateUser()));
			}
		}
		return attachments;
	}

	@Override
	public List<Proposal> fetchProposalsByGrantCallIdAndStatus(Integer grantCallId, List<Integer> proposalStatus) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Proposal> query = builder.createQuery(Proposal.class);
		Root<Proposal> rootProposal = query.from(Proposal.class);
		Predicate predicateGrantCallId = builder.equal(rootProposal.get(GRANT_CALL_ID), grantCallId);
		Predicate predicateProposalStatus = builder.not(rootProposal.get(STATUS_CODE).in(proposalStatus));
		query.where(builder.and(predicateGrantCallId, predicateProposalStatus));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Proposal> fetchProposalsOfGrantCall(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Proposal> query = builder.createQuery(Proposal.class);
		Root<Proposal> rootProposal = query.from(Proposal.class);
		Predicate predicateGrantCallId = builder.equal(rootProposal.get(GRANT_CALL_ID), grantCallId);
		query.where(builder.and(predicateGrantCallId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Proposal> fetchProposalsByGrantCallIdAndStatus(Integer grantCallId, List<Integer> proposalStatus, String homeUnitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Proposal> query = builder.createQuery(Proposal.class);
		Root<Proposal> rootProposal = query.from(Proposal.class);
		Predicate predicateGrantCallId = builder.equal(rootProposal.get(GRANT_CALL_ID), grantCallId);
		Predicate predicateProposalStatus = builder.not(rootProposal.get(STATUS_CODE).in(proposalStatus));
		Predicate predicateHomeUnitNumber = builder.equal(rootProposal.get("homeUnitNumber"), homeUnitNumber);
		if (homeUnitNumber == null) {
			query.where(builder.and(predicateGrantCallId, predicateProposalStatus));
		} else {
			query.where(builder.and(predicateGrantCallId, predicateProposalStatus, predicateHomeUnitNumber));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalPersonAttachment fetchPersonAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(ProposalPersonAttachment.class, attachmentId);
	}

	@Override
	public Boolean fetchEligibilityCriteria(Integer proposalId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String checkEligibilityCriteriaMet = "SELECT bc.isEligibilityCriteriaMet FROM Proposal bc WHERE bc.proposalId = :proposalId";
			Query query = session.createQuery(checkEligibilityCriteriaMet);
			query.setParameter(PROPOSAL_ID, proposalId);
			return query.getSingleResult() != null ? Boolean.TRUE : Boolean.FALSE;
		} catch (Exception e) {
			return Boolean.FALSE;
		}
	}

	@Override
	public void deleteProposalBudgetRate(List<FibiProposalRate> proposalRateList) {
		try {
			hibernateTemplate.deleteAll(proposalRateList);
		} catch (Exception e) {
			logger.error("exception in deleteProposalBudgetRate: {} ", e.getMessage());
		}		
	}

	@Override
	public List<ProposalPerson> fetchProposalPersonBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();		
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPerson> query = builder.createQuery(ProposalPerson.class);
		Root<ProposalPerson> rootProposalPerson = query.from(ProposalPerson.class);
		query.where(builder.equal(rootProposalPerson.get(PROPOSAL_ID),proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalPersonAttachment> fetchProposalPersonAttachmentBasedOnProposalPersonId(Set<Integer> proposalPersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonAttachment> query = builder.createQuery(ProposalPersonAttachment.class);
		Root<ProposalPersonAttachment> rootProposalPersonAttachment = query.from(ProposalPersonAttachment.class);
		query.where(rootProposalPersonAttachment.get("proposalPerson").get("proposalPersonId").in(proposalPersonId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer fetchGrantTypeCodeBasedOnProposalId(Integer proposalId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String grantTypeCode = "SELECT grantTypeCode FROM Proposal WHERE proposalId=:proposalId";
			Query query = session.createQuery(grantTypeCode);
			query.setParameter(PROPOSAL_ID, proposalId);
			return Integer.parseInt(query.getSingleResult().toString());
		}catch (Exception e) {
			logger.error("exception in fetchGrantTypeCodeBasedOnProposalId: {} ", e.getMessage());
			return null;
		}
	}

	@Override
	public String getProposalName(Integer proposalId) {
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String title = "SELECT title FROM Proposal WHERE proposalId=:proposalId";
		Query query = session.createQuery(title);
		query.setParameter(PROPOSAL_ID, proposalId);
		String proposalTitle = query.getSingleResult().toString();
		return proposalTitle == null ? null : proposalTitle;
		} catch (Exception e) {
			logger.error("exception in proposal: {} ", e.getMessage());
			return null;
		}
	}

	@Override
	public ProposalPersonRoles fetchProposalAggregatorBasedOnPersonIdAndProposalId(String personId, Integer proposalId) {
		List<ProposalPersonRoles> proposalPersonRoles = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonRoles> query = builder.createQuery(ProposalPersonRoles.class);
		Root<ProposalPersonRoles> rootProposalPersonRoles = query.from(ProposalPersonRoles.class);
		Predicate predicatePersonId = builder.equal(rootProposalPersonRoles.get("personId"), personId);
		Predicate predicateProposalId = builder.equal(rootProposalPersonRoles.get(PROPOSAL_ID), proposalId);
		Predicate predicateRoleId = builder.equal(rootProposalPersonRoles.get("roleId"), Constants.PROPOSAL_AGGREGATOR_ROLE_ID);
		query.where(builder.and(predicatePersonId, predicateProposalId, predicateRoleId));
		proposalPersonRoles = session.createQuery(query).getResultList();
		if (proposalPersonRoles != null && !proposalPersonRoles.isEmpty()) {
			return proposalPersonRoles.get(0);
		}
		return null;
	}

	@Override
	public ProposalPersonRole fetchProposalPersonRoles(Integer proposalPersonRoleId) {
		return hibernateTemplate.get(ProposalPersonRole.class, proposalPersonRoleId);
	}

	@Override
	public void deleteProposalPersonAttachment(Integer attachmentId) {
		hibernateTemplate.delete(fetchPersonAttachmentById(attachmentId));
	}

	@Override
	public ProposalKPI saveOrUpdateProposalKPI(ProposalKPI proposalKpi) {
		try {
			hibernateTemplate.saveOrUpdate(proposalKpi);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalKPI: {} ", e.getMessage());
		}
		return proposalKpi;
	}

	@Override
	public ProposalKPICriteria saveOrUpdateProposalKPICriterias(ProposalKPICriteria proposalKpiCriteria) {
		try {
			hibernateTemplate.saveOrUpdate(proposalKpiCriteria);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalKPICriterias: {} ", e.getMessage());
		}
		return proposalKpiCriteria;
	}

	@Override
	public List<ProposalKPI> fetchAllProposalKPI(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalKPI> query = builder.createQuery(ProposalKPI.class);
		Root<ProposalKPI> rootProposalKpi = query.from(ProposalKPI.class);
		query.where(builder.equal(rootProposalKpi.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}
    
	@Override
	public ProposalExtension saveOrUpdateProposalExtension(ProposalExtension proposalExtension) {
		try {
			hibernateTemplate.saveOrUpdate(proposalExtension);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalExtension: {} ", e.getMessage());
		}
		return proposalExtension;
	}

	@Override
	public ProposalExtension fetchProposalExtensionById(Integer proposalId) {
		return hibernateTemplate.get(ProposalExtension.class, proposalId);

    }

	@Override
	public Integer getScoreByWorkflowDetailId(Integer workFlowDetailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String score = "SELECT score FROM WorkflowReviewerScore WHERE workflowDetailId=:workFlowDetailId";
		Query query = session.createQuery(score);
		query.setParameter("workFlowDetailId", workFlowDetailId);
		return query.getSingleResult() == null ? null : Integer.parseInt(query.getSingleResult().toString());
	}

	@Override
	public String deleteProposalKPI(ProposalKPI proposalKPI) {
		hibernateTemplate.delete(proposalKPI);
		return "Proposal KPI deleted successfully";
	}

	@Override
	public Boolean fetchPersonCanScore(Integer proposalId, String personId, Integer workflowDetailId) {
		Boolean isPersonCanScore = false;
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String canScore = "SELECT P1.canScore FROM Workflow W1 LEFT OUTER JOIN WorkflowDetail W2 ON W1.workflowId=W2.workflowId LEFT OUTER JOIN ProposalEvaluationPanel P1 ON P1.mapId=W2.mapId AND P1.proposalId = W1.moduleItemId WHERE W1.moduleCode= 3 AND W1.moduleItemId= :proposalId AND W1.isWorkflowActive='Y' AND W2.approverPersonId = :personId AND W2.workflowDetailId = :workflowdetailId AND W2.approvalStatusCode NOT IN('B','K','A') group by W1.workflowId";
			Query query = session.createQuery(canScore);
			query.setParameter(PROPOSAL_ID, proposalId.toString());
			query.setParameter("personId", personId);
			query.setParameter("workflowdetailId", workflowDetailId);
			isPersonCanScore = (Boolean) query.getSingleResult();
			if (isPersonCanScore == null) {
				isPersonCanScore = false;
			}
		} catch (NoResultException e) {
			isPersonCanScore = false;
		}
		return isPersonCanScore;
	}

	@Override
	public WorkflowReviewerScore saveOrUpdateWorkflowScoreDetail(WorkflowReviewerScore workflowReviewerScore) {
		try {
			hibernateTemplate.saveOrUpdate(workflowReviewerScore);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateWorkflowScoreDetail: {} ", e.getMessage());
		}
		return workflowReviewerScore;
	}

	@Override
	public WorkflowReviewerAttachment fetchWorkflowReviewerAttachmentById(Integer workflowReviewerAttmntsId) {
		return hibernateTemplate.get(WorkflowReviewerAttachment.class, workflowReviewerAttmntsId);
	}

	@Override
	public List<WorkflowReviewerScore> fetchAllWorkflowReviewerDetails(Integer workflowDetailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkflowReviewerScore> query = builder.createQuery(WorkflowReviewerScore.class);
		Root<WorkflowReviewerScore> rootWorkflowReviewerScore= query.from(WorkflowReviewerScore.class);
		query.where(builder.equal(rootWorkflowReviewerScore.get("workflowDetailId"), workflowDetailId));
		return session.createQuery(query).getResultList();
	}

	public boolean checkForProposalEvaluationPanelRank(Integer mapId, Integer proposalId) {
		ProposalEvaluationPanel proposalEvaluationPanel = fetchProposalEvaluationPanelBasedOnMapId(mapId, proposalId);
		return proposalEvaluationPanel != null && proposalEvaluationPanel.getCanScore();
	}

	private ProposalEvaluationPanel fetchProposalEvaluationPanelBasedOnMapId(Integer mapId, Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalEvaluationPanel> query = builder.createQuery(ProposalEvaluationPanel.class);
		Root<ProposalEvaluationPanel> rootProposalEvaluationPanel= query.from(ProposalEvaluationPanel.class);
		Predicate predicateMapId = builder.equal(rootProposalEvaluationPanel.get("mapId"), mapId);
		Predicate predicateProposalId = builder.equal(rootProposalEvaluationPanel.get(PROPOSAL_ID), proposalId);
		query.where(builder.and(predicateMapId, predicateProposalId));
		return session.createQuery(query).uniqueResult();
	}


	@Override
	public ProposalEvaluationScore fetchEvaluationScoreByGrantCallandProposalId(Integer grantCallId,Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalEvaluationScore> query = builder.createQuery(ProposalEvaluationScore.class);
		Root<ProposalEvaluationScore> rootProposalEvaluationScore = query.from(ProposalEvaluationScore.class);
		Predicate predicateGrantHeaderId = builder.equal(rootProposalEvaluationScore.get("grantHeaderId"), grantCallId);
		Predicate predicateProposalId = builder.equal(rootProposalEvaluationScore.get(PROPOSAL_ID), proposalId);
		query.where(builder.and(predicateGrantHeaderId, predicateProposalId));
		query.orderBy(builder.desc(rootProposalEvaluationScore.get("proposalEvalutionScoreId")));
		return session.createQuery(query).list().get(0);
	}
	
	@Override
	public BigDecimal fetchMaximumBudget(Integer proposalId) {
		BigDecimal totalCost = null;
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String totalCostQuery = "SELECT totalCost FROM BudgetHeader where isFinalBudget='Y' and proposalId = :proposalId";
			Query query = session.createQuery(totalCostQuery);
			query.setParameter(PROPOSAL_ID, proposalId);
			totalCost = (BigDecimal) query.getSingleResult();
		} catch (NoResultException e) {
			totalCost = null;
		}
		return totalCost;
	}

	@Override
	public List<Proposal> fetchProposalsByGrantCallIdAndProposalStatus(Integer grantCallId, List<Integer> proposalStatus) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Proposal> query = builder.createQuery(Proposal.class);
		Root<Proposal> proposal = query.from(Proposal.class);
		Predicate predicateGrantCallId = builder.equal(proposal.get(GRANT_CALL_ID), grantCallId);
		Predicate predicateProposalStatus = (builder.in(proposal.get(STATUS_CODE)).value(proposalStatus));
		query.where(builder.and(predicateGrantCallId), predicateProposalStatus);
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Proposal> fetchSubmittedProposalList(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<Proposal> submittedProposals = new ArrayList<>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
			String functionName = "GET_GRANTCALL_SUBMTD_PROPOSALS";
			String functionCall = "{call " + functionName + "(?) }";
			statement = connection.prepareCall(functionCall);
			statement.setInt(1, grantCallId);
			statement.execute();
			resultSet = statement.getResultSet();
			}else if (oracledb.equalsIgnoreCase("Y")) {
				String functionName = "GET_GRANTCALL_SUBMTD_PROPOSALS";
				String functionCall = "{call " + functionName + "(?,?) }";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, grantCallId);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				Proposal proposal = new Proposal();
				proposal.setProposalId(resultSet.getInt("PROPOSAL_ID"));
				proposal.setTitle(resultSet.getString("TITLE"));
				proposal.setHomeUnitName(resultSet.getString("HOME_UNIT_NAME"));
				proposal.setHomeUnitNumber(resultSet.getString("HOME_UNIT_NUMBER"));
				ProposalPerson investigator = new ProposalPerson();
				investigator.setFullName(resultSet.getString("PI"));
				investigator.setPersonId(resultSet.getString("PI_PERSON_ID"));
				investigator.setPrimaryTitle(resultSet.getString("PRIMARY_TITLE"));
				proposal.setInvestigator(investigator);
				proposal.setTotalCost(resultSet.getBigDecimal("BUDGET"));
				ProposalStatus proposalStatus = new ProposalStatus();
				proposalStatus.setDescription(resultSet.getString("PROPOSAL_STATUS"));
				proposalStatus.setStatusCode(resultSet.getInt("PROPOSAL_STATUS_CODE"));
				proposal.setProposalStatus(proposalStatus);
				ProposalEvaluationScore proposalEvaluationScore = new ProposalEvaluationScore();
				if (resultSet.getString("ADJUSTED_SCORE") != null) {
					proposalEvaluationScore.setAdjustedScore(resultSet.getBigDecimal("ADJUSTED_SCORE"));
					proposalEvaluationScore.setProposalEvalutionScoreId(resultSet.getInt("EVALUATION_SCORE_ID"));
					proposalEvaluationScore.setGrantHeaderId(grantCallId);
					proposalEvaluationScore.setProposalId(resultSet.getInt("PROPOSAL_ID"));
					proposalEvaluationScore.setIsShortListed(resultSet.getString("IS_SHORTLISTED"));
					proposalEvaluationScore.setProposalRank(resultSet.getString("PROPOSAL_RANK") == null ? "" : resultSet.getString("PROPOSAL_RANK"));
				}
				proposal.setProposalEvaluationScore(proposalEvaluationScore);
				if (resultSet.getString("TOTAL") != null) {
					proposal.setTotal(resultSet.getBigDecimal("TOTAL"));
				}
				if (resultSet.getString("AVERAGE_SCORE") != null) {
					proposal.setScore(resultSet.getBigDecimal("AVERAGE_SCORE"));
				}
				if (resultSet.getString("MAP_ID") != null) {
					proposal.setScoringMapId(resultSet.getInt("MAP_ID"));
				}
				proposal.setDuration(resultSet.getString("DURATION"));
				proposal.setProposalCategory(resultSet.getString("CATEGORY"));
				proposal.setProposalTypeDescription(resultSet.getString("PROPOSAL_TYPE"));
				submittedProposals.add(proposal);
			}
		} catch (SQLException e) {
			logger.error("exception : {} ", e.getMessage());
		} catch (Exception e) {
			logger.error("exception : {} ", e.getMessage());
		} finally {
			try {
				resultSet.close();
			} catch (SQLException e) {
				logger.error("exception : {} ", e.getMessage());					
			}
		}
		return submittedProposals;
	}

	@Override
	public List<WorkflowReviewerScore> fetchWorkflowReviewer(Integer workflowDetailId, String scoringCriteriaTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkflowReviewerScore> query = builder.createQuery(WorkflowReviewerScore.class);
		Root<WorkflowReviewerScore> rootWorkflowReviewerScore = query.from(WorkflowReviewerScore.class);
		Predicate predicate1 = builder.equal(rootWorkflowReviewerScore.get("workflowDetailId"), workflowDetailId);
		Predicate predicate2 = builder.equal(rootWorkflowReviewerScore.get("scoringCriteriaTypeCode"),
				scoringCriteriaTypeCode);
		query.where(builder.and(predicate1, predicate2));
		query.orderBy(builder.desc(rootWorkflowReviewerScore.get("scoringCriteriaTypeCode")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalComment saveOrUpdateProposalComment(ProposalComment proposalComment) {
		hibernateTemplate.saveOrUpdate(proposalComment);
		return proposalComment;
	}

	@Override
	public List<ProposalComment> fetchProposalCommentsByParams(Integer proposalId, String userName, Boolean isViewPrivateComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalComment> query = builder.createQuery(ProposalComment.class);
		Root<ProposalComment> proposalComment = query.from(ProposalComment.class);
		Predicate predicateUser = builder.equal(proposalComment.get(UPDATE_USER), userName);
		Predicate predicateIsPrivate = builder.equal(proposalComment.get("isPrivate"), true);
		Predicate predicateProposalId = builder.equal(proposalComment.get(PROPOSAL_ID), proposalId);
		Predicate privateComments = builder.and(predicateIsPrivate, predicateProposalId);
		Predicate publicComments = builder.and(builder.not(predicateIsPrivate), predicateProposalId);
		if (Boolean.FALSE.equals(isViewPrivateComment)) {
			privateComments = builder.and(privateComments, predicateUser);
		}
		query.where(builder.or(privateComments, publicComments));
		query.orderBy(builder.desc(proposalComment.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> fetchMainPanelPersonIdsByGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String approverPersonId = "SELECT G2.approverPersonId FROM  GrantCallEvaluationPanel G1 LEFT OUTER JOIN WorkflowMapDetail G2 ON G1.mapId=G2.mapId WHERE G1.grantCallId=:grantCallId and G1.isMainPanel='Y'";
		Query query = session.createQuery(approverPersonId);
		query.setParameter(GRANT_CALL_ID, grantCallId);
		return query.getResultList();
	}

	@Override
	public boolean isKeywordExist(String scienceKeyword) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String approverPersonId = "SELECT count(*) FROM  ScienceKeyword WHERE description=:scienceKeyword";
		Query query = session.createQuery(approverPersonId);
		query.setParameter("scienceKeyword", scienceKeyword);
		return Integer.parseInt(query.getSingleResult().toString()) > 0;
	}

	@Override
	public List<Proposal> fetchProposalsByStatusCode(Integer proposalStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Proposal> proposal = builder.createQuery(Proposal.class);
		Root<Proposal> rootProposal = proposal.from(Proposal.class);
		proposal.where(builder.equal(rootProposal.get("statusCode"), proposalStatusCode));
		return session.createQuery(proposal).getResultList();
	}

	@Override
	public List<ProposalPersonRoles> fetchProposalPersonRolesByParams(String personId, Integer proposalId, List<Integer> roleIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonRoles> query = builder.createQuery(ProposalPersonRoles.class);
		Root<ProposalPersonRoles> proposalPersonRoles = query.from(ProposalPersonRoles.class);
		Predicate predicatePersonId = builder.equal(proposalPersonRoles.get("personId"), personId);
		Predicate predicateProposalId = builder.equal(proposalPersonRoles.get("proposalId"), proposalId);
		if (roleIds != null && !roleIds.isEmpty()) {
			Predicate predicateRoleIds = proposalPersonRoles.get("roleId").in(roleIds);
			query.where(builder.and(predicatePersonId, predicateProposalId, predicateRoleIds));
		} else {
			query.where(builder.and(predicatePersonId, predicateProposalId));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteProposalMileStones(List<ProposalMileStone> proposalMileStones) {
		hibernateTemplate.deleteAll(proposalMileStones);
	}

	@Override
	public void deleteWorkflowScoreComments(Integer workflowReviewerCommentsId) {
		hibernateTemplate.delete(hibernateTemplate.get(WorkflowReviewerComment.class, workflowReviewerCommentsId));
	}

	@Override
	public void deleteWorkflowReviewerAttachment(Integer workflowReviewerAttmntsId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<WorkflowReviewerAttachment> delete = builder.createCriteriaDelete(WorkflowReviewerAttachment.class);
		Root<WorkflowReviewerAttachment> root = delete.from(WorkflowReviewerAttachment.class);
		delete.where(builder.equal(root.get("workflowReviewerAttmntsId"), workflowReviewerAttmntsId));
		session.createQuery(delete).executeUpdate();
	}

	@Override
	public Integer getGrantCallIdByProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<Proposal> rootProposal = query.from(Proposal.class);
		query.where(builder.equal(rootProposal.get("proposalId"), proposalId));
		query.select(rootProposal.get("grantCallId"));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public Boolean isPersonHasRightInProposal(String personId, List<String> rightName, Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String approverPersonId = new StringBuilder("FROM  ProposalPersonRoles T1 ")
				.append("INNER JOIN RoleRights T2 ON T1.roleId=T2.roleId ")
				.append("INNER JOIN Rights T3 ON T3.rightId = T2.rightId where T1.proposalId =:proposalId and T1.personId =:personId ")
				.append("and T3.rightName in(:rightName)").toString();
		Query query = session.createQuery(approverPersonId);
		query.setParameter("personId", personId);
		query.setParameter("rightName", rightName);
		query.setParameter("proposalId", proposalId);
		return !query.getResultList().isEmpty();
	}

	@Override
	public List<ProposalOrganization> loadProposalOrganization(Integer proposalId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ProposalOrganization> criteria = builder.createQuery(ProposalOrganization.class);
			Root<ProposalOrganization> proposalRoot = criteria.from(ProposalOrganization.class);
			criteria.where(builder.equal(proposalRoot.get("proposalId"), proposalId));
			criteria.orderBy(builder.asc(proposalRoot.get("organizationTypeCode")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public void saveOrUpdateProposalOrganization(ProposalOrganization proposalOrganization) {
		hibernateTemplate.saveOrUpdate(proposalOrganization);
	}

	@Override
	public void saveOrUpdateProposalCongDistrict(ProposalCongDistrict proposalCongDistrict) {
		hibernateTemplate.saveOrUpdate(proposalCongDistrict);
	}

	@Override
	public void saveOrUpdateCongDistrict(CongressionalDistrict congressionalDistrict) {
		hibernateTemplate.saveOrUpdate(congressionalDistrict);
	}

	@Override
	public void deleteProposalOrganization(Integer proposalOrganizationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String deleteProposalCongDistrict = new StringBuilder("delete from ProposalCongDistrict t1  ")
				.append("where t1.proposalOrganization.proposalOrganizationId =: proposalOrganizationId ").toString();
		Query queryDist = session.createQuery(deleteProposalCongDistrict);
		queryDist.setParameter("proposalOrganizationId", proposalOrganizationId);
		queryDist.executeUpdate();
		String deleteProposalOrganization = new StringBuilder("delete from ProposalOrganization t1  ")
				.append("where t1.proposalOrganizationId =: proposalOrganizationId ").toString();
		Query queryOrg = session.createQuery(deleteProposalOrganization);
		queryOrg.setParameter("proposalOrganizationId", proposalOrganizationId);
		queryOrg.executeUpdate();
	}

	@Override
	public void deleteProposalCongDistrict(Integer proposalCongDistrictId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String deleteProposalCongDistrict = new StringBuilder("delete from ProposalCongDistrict t1  ")
				.append("where t1.proposalCongDistrictId =: proposalCongDistrictId ").toString();
		Query queryDist = session.createQuery(deleteProposalCongDistrict);
		queryDist.setParameter("proposalCongDistrictId", proposalCongDistrictId);
		queryDist.executeUpdate();
	}

	@Override
	public Boolean checkUnitNumberHasRight(String personId, String unitNumber, String rights) {		
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoleRT> query = builder.createQuery(PersonRoleRT.class);
		Root<PersonRoleRT> rootPersonRole = query.from(PersonRoleRT.class);
		Predicate predicate1 = builder.equal(rootPersonRole.get("personRoleRTAttributes").get("personId"), personId);
		Predicate predicate2 = builder.equal(rootPersonRole.get("personRoleRTAttributes").get("unitNumber"), unitNumber);
		Predicate predicate3 = rootPersonRole.get("personRoleRTAttributes").get("rightName").in(rights);
		query.where(builder.and(predicate1, predicate2, predicate3));
		query.distinct(true);
		return (!(session.createQuery(query).list()).isEmpty() && (session.createQuery(query).list()) != null);
	}

	@Override
	public Boolean isPersonHasRightInProposal(String personId, String rightName, Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String approverPersonId = new StringBuilder("FROM ProposalPersonRoles T1 ")
				.append(" INNER JOIN RoleRights T2 ON T1.roleId=T2.roleId ")
				.append(" INNER JOIN Rights T3 ON T3.rightId = T2.rightId where T1.proposalId =:proposalId and T1.personId =:personId ")
				.append(" and T3.rightName =:rightName").toString();
		Query query = session.createQuery(approverPersonId);
		query.setParameter("personId", personId);
		query.setParameter("rightName", rightName);
		query.setParameter("proposalId", proposalId);
		return !query.getResultList().isEmpty();
	}

	@Override
	public ProposalKeyword saveOrUpdateProposalKeyword(ProposalKeyword proposalKeyword) {
		try {
			hibernateTemplate.saveOrUpdate(proposalKeyword);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalKeyword: {} ", e.getMessage());
		}
		return proposalKeyword;
	}

	@Override
	public void deleteProposalKeyWords(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String deleteProposalKeyword = new StringBuilder("delete from ProposalKeyword t1  ")
				.append("where t1.proposal.proposalId =: proposalId ").toString();
		Query queryDist = session.createQuery(deleteProposalKeyword);
		queryDist.setParameter("proposalId", proposalId);
		queryDist.executeUpdate();
	}

	@Override
	public void deleteProposalMilestone(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String deleteProposalMilestone = new StringBuilder("delete from ProposalMileStone t1  ")
				.append("where t1.proposalId =: proposalId ").toString();
		Query queryDist = session.createQuery(deleteProposalMilestone);
		queryDist.setParameter("proposalId", proposalId);
		queryDist.executeUpdate();
	}

	@Override
	public String getScienceKeyword(String description) {
		try {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT code FROM ScienceKeyword where description = :description");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("description", description);
		return (String) query.getSingleResult();
		} catch (Exception e) {
			logger.error("error in getScienceKeyword {}", e.getMessage());
			return null;
		}
	}

	@Override
	public String getKpiTypeBasedOnParam(String description) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("SELECT kpiTypeCode FROM KPIType where description =:description");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("description", description);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			logger.error("error in getKpiTypeBasedOnParam {}", e.getMessage());
			return null;
		}
	}

	@Override
	public String getkpiCrieriaBasedOnParam(String description) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("SELECT kpiCriteriaTypeCode FROM KPICriteriaType where description = :description");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("description", description);
			Object kpiCriteriaTypeCode = query.getSingleResult();
			if (kpiCriteriaTypeCode != null) {
				return (String) kpiCriteriaTypeCode;
			}
		} catch (Exception e) {
			logger.error("error in getkpiCrieriaBasedOnParam {}", e.getMessage());
		}
		return null;
	}

	@Override
	public String getSpecialReviewType(String description) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("SELECT specialReviewTypeCode FROM SpecialReviewType where description = :description");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("description", description);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			logger.error("error in getkpiCrieriaBasedOnParam {}", e.getMessage());
			return null;
		}
	}

	@Override
	public void deleteProposalKPIs(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalKPI> query = builder.createQuery(ProposalKPI.class);
		Root<ProposalKPI> rootProposalKPI = query.from(ProposalKPI.class);
		Predicate predicate1 = builder.equal(rootProposalKPI.get("proposalId"), proposalId);
		query.where(builder.and(predicate1));
		hibernateTemplate.deleteAll(session.createQuery(query).getResultList());
	}

	@Override
	public String fetchProposalLeadUnitNumberByProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT homeUnitNumber FROM Proposal WHERE proposalId=:proposalId";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("proposalId", proposalId);
		return query.getSingleResult();
	}

	@Override
	public List<ProposalPersonAttachment> loadProposalKeyPersonAttachments(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonAttachment> query = builder.createQuery(ProposalPersonAttachment.class);
		Root<ProposalPersonAttachment> rootProposalPersonAttachment = query.from(ProposalPersonAttachment.class);
		Predicate predicateProposalId = builder.equal(rootProposalPersonAttachment.get("proposalPerson").get("proposalId"), proposalId);
		query.where(builder.and(predicateProposalId));
		query.orderBy(builder.desc(rootProposalPersonAttachment.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalHistory saveOrUpdateProposalHistory(ProposalHistory proposalHistory) {
		try {
			hibernateTemplate.saveOrUpdate(proposalHistory);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateproposalHistory : {} ", e.getMessage());
		}
		return proposalHistory;
	}

	@Override
	public List<ProposalHistory> getProposalHistoryBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalHistory> query = builder.createQuery(ProposalHistory.class);
		Root<ProposalHistory> rootProposalHistory = query.from(ProposalHistory.class);
		Predicate predicate1 = builder.equal(rootProposalHistory.get("activeProposalId"), proposalId);
		query.where(builder.and(predicate1));
		query.orderBy(builder.desc(rootProposalHistory.get("historyId")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void updateDocumentStatusCode(Integer proposalId, String documentStatusCode) throws Exception {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaUpdate<Proposal> criteriaUpdate = cb.createCriteriaUpdate(Proposal.class);
			Root<Proposal> root = criteriaUpdate.from(Proposal.class);
			criteriaUpdate.set("documentStatusCode", documentStatusCode);
			criteriaUpdate.set(UPDATE_TIMESTAMP, commonDao.getCurrentTimestamp());
			criteriaUpdate.set(UPDATE_USER, AuthenticatedUser.getLoginUserName());
			criteriaUpdate.where(cb.equal(root.get("proposalId"), proposalId));
			session.createQuery(criteriaUpdate).executeUpdate();
		} catch (Exception e) {
			logger.error("Error in completeProposalAdminCorrection {}", e.getMessage());
		}		
	}

	@Override
	public Boolean getIsFileDataIdFound(String fileId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<Long> query = cb.createQuery(Long.class);
			Root<ProposalAttachment> root = query.from(ProposalAttachment.class);
			query.select(cb.count(root));
			query.where(cb.equal(root.get("fileDataId"), fileId));
			Long count = session.createQuery(query).getSingleResult();
			return count.intValue() > 1 ? Boolean.TRUE : Boolean.FALSE;
		} catch (Exception e) {
			return Boolean.TRUE;
		}
	}

	@Override
	public String getProposalTitleByProposalId(Integer proposalId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT title FROM Proposal WHERE proposalId=:proposalId";
			javax.persistence.Query query = session.createQuery(hqlQuery);
			query.setParameter("proposalId", proposalId);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			logger.error("Exception in getProposalTitleByProposalId {}" , e.getMessage());
			return "";
		}
	}

	@Override
	public ProposalDocumentStatus fetchProposalDocumentStatus(String documentStatusCode) {
		return hibernateTemplate.get(ProposalDocumentStatus.class, documentStatusCode);
	}

	@Override
	public HashMap<String, Object> checkGrantCallEligibilty(Integer grantCallId, String personId, Integer piRoleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet rset = null;
		try {
			if (oracledb.equals("Y")) {
				String procedureName = "GET_GRANTCALL_ELIGIBLITY_STATUS";
				String procedureCall = "{call " + procedureName + "(?,?,?,?)}";
				callstm = connection.prepareCall(procedureCall);
				callstm.registerOutParameter(1, OracleTypes.CURSOR);
				callstm.setInt(2, grantCallId);
				callstm.setInt(3, piRoleCode);
				callstm.setString(4, personId);
				callstm.execute();
				rset = (ResultSet) callstm.getObject(1);
			} else if (oracledb.equals("N")) {
				callstm = connection.prepareCall("{call GET_GRANTCALL_ELIGIBLITY_STATUS(?,?,?)}");
				callstm.setInt(1, grantCallId);
				callstm.setInt(2, piRoleCode);
				callstm.setString(3, personId);
				callstm.execute();
				rset = callstm.getResultSet();
			}
			while (rset.next()) {
				HashMap<String, Object> eligibilityMap = new HashMap<>();
				eligibilityMap.put("status", rset.getString("STATUS"));
				eligibilityMap.put("message", rset.getString("MESSAGE"));
				return eligibilityMap;
			}
		} catch (SQLException e) {
			logger.error("Exception in checkGrantCallEligibilty : {}", e.getMessage());
		} finally {
			if (callstm != null) {
				try {
					callstm.close();
				} catch (SQLException e) {
					logger.error("Exception in checkGrantCallEligibilty callstm close : {}", e.getMessage());
				}
			}
		}
		return new HashMap<>();
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> loadProposalKeyPersonnelPersons(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder("SELECT proposalPerson.proposalPersonId, proposalPerson.fullName ");
		hqlQuery.append("FROM ProposalPerson proposalPerson " );
		hqlQuery.append("WHERE proposalPerson.proposalId=:proposalId order by fullName asc");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("proposalId", proposalId);
		return query.getResultList();
	}

	@Override
	public void deleteKeyPersonnelAttachment(Integer attachmentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder("DELETE FROM ProposalPersonAttachment attachment WHERE attachment.attachmentId = :attachmentId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("attachmentId", attachmentId);
		query.executeUpdate();
	}

	@Override
	public List<ProposalPersonAttachment> loadProposalKeyPersonAttachments(Integer proposalId, Integer documentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonAttachment> query = builder.createQuery(ProposalPersonAttachment.class);
		Root<ProposalPersonAttachment> rootProposalPersonAttachment = query.from(ProposalPersonAttachment.class);
		Predicate predicateProposalId = builder.equal(rootProposalPersonAttachment.get("proposalPerson").get("proposalId"), proposalId);
		Predicate predicate2 = builder.equal(rootProposalPersonAttachment.get("documentId"), documentId);
		query.where(builder.and(predicateProposalId, predicate2));
		query.orderBy(builder.desc(rootProposalPersonAttachment.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}
	
	@Override
	public void updateProposalPersonCertification(Integer proposalId, String personId, Boolean status, Boolean nonEmployee) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<ProposalPerson> criteriaUpdate = cb.createCriteriaUpdate(ProposalPerson.class);
		Root<ProposalPerson> root = criteriaUpdate.from(ProposalPerson.class);
		criteriaUpdate.set(UPDATE_TIMESTAMP, commonDao.getCurrentTimestamp());
		criteriaUpdate.set(UPDATE_USER, AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("personCertified", status);
		Predicate predicateProposalId = cb.equal(root.get("proposalId"), proposalId);
		Predicate predicatePersonId = null;
		if (Boolean.TRUE.equals(nonEmployee)) {
			predicatePersonId = cb.equal(root.get("rolodexId"), personId);
		} else {
			predicatePersonId = cb.equal(root.get("personId"), personId);
		} 
		criteriaUpdate.where(cb.and(predicateProposalId, predicatePersonId));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public List<ProposalPerson> proposalPersonsForCertification(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPerson> query = builder.createQuery(ProposalPerson.class);
		Root<ProposalPerson> rootProposalPerson = query.from(ProposalPerson.class);
		Predicate predicateProposalId = builder.equal(rootProposalPerson.get(PROPOSAL_ID), proposalId);
		Predicate predicateCertificationFlag = builder.notEqual(rootProposalPerson.get("proposalPersonRole").get("certificationRequired"), "N");
		Predicate predicateRolodexId = builder.isNull(rootProposalPerson.get("rolodexId"));
		query.orderBy(builder.asc(rootProposalPerson.get("proposalPersonRole").get("sortId")));
		query.where(builder.and(predicateProposalId, predicateCertificationFlag,predicateRolodexId));
		return session.createQuery(query).getResultList();	
	}

	@Override
	public Boolean isAllProposalPersonCertified(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<ProposalPerson> rootProposalPerson = query.from(ProposalPerson.class);
		Predicate predicateProposalId = builder.equal(rootProposalPerson.get(PROPOSAL_ID), proposalId);
		Predicate predicateCertificationFlag = builder.notEqual(rootProposalPerson.get("proposalPersonRole").get("certificationRequired"), "N");
		Predicate predicateRolodexId = builder.isNull(rootProposalPerson.get("rolodexId"));
		Predicate predicateCertificationCertifiedFalse = builder.equal(rootProposalPerson.get("personCertified"), false);
		Predicate predicateCertificationCertifiedNull = builder.isNull(rootProposalPerson.get("personCertified"));
		Predicate predicateCertificationCertified = builder.or(predicateCertificationCertifiedFalse, predicateCertificationCertifiedNull);
		query.orderBy(builder.asc(rootProposalPerson.get("proposalPersonRole").get("sortId")));
		query.select(rootProposalPerson.get("proposalPersonId"));
		query.where(builder.and(predicateProposalId, predicateCertificationFlag, predicateCertificationCertified,predicateRolodexId));
		return session.createQuery(query).getResultList().isEmpty();
	}

	@Override
	public String isProposalPersonTrainingCompleted(String personId, Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		String result = null;
		try {
			String functionName = "FN_PERSON_TRAINING_COMPLETED";
			String functionCall = "{ ? = call " + functionName + "(?,?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.VARCHAR);
			statement.setString(2, personId);
			statement.setInt(3, proposalId);
			statement.execute();
			result = statement.getString(1);
		} catch (SQLException e) {
			logger.error("Exception in isProposalPersonTrainingCompleted : {}", e.getMessage());
		} finally {
			if (statement != null) {
				try {
					statement.close();
				} catch (SQLException e) {
					logger.error("Exception in isProposalPersonTrainingCompleted callstm close : {}", e.getMessage());
				}
			}
		}
		return result;
	}

	@Override
	public ProposalCommentAttachment saveOrUpdateProposalCommentAttachment(ProposalCommentAttachment proposalCommentAttachment) {
		try {
			hibernateTemplate.saveOrUpdate(proposalCommentAttachment);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return proposalCommentAttachment;
	}

	@Override
	public List<ProposalCommentAttachment> fetchProposalCommentAttachments(Integer proposalCommentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalCommentAttachment> query = builder.createQuery(ProposalCommentAttachment.class);
		Root<ProposalCommentAttachment> rootProposalCommentAttachment = query.from(ProposalCommentAttachment.class);
		query.where(builder.and(builder.equal(rootProposalCommentAttachment.get("proposalCommentId"), proposalCommentId)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalCommentAttachment getProposalCommentAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(ProposalCommentAttachment.class, attachmentId);
	}

	@Override
	public void deleteProposalComment(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<ProposalComment> delete = builder.createCriteriaDelete(ProposalComment.class);
		Root<ProposalComment> root = delete.from(ProposalComment.class);
		delete.where(builder.equal(root.get("proposalId"), proposalId));
		session.createQuery(delete).executeUpdate();	
	}

	@Override
	public void deleteProposalCommentAttachement(List<Integer> proposalCommentIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<ProposalCommentAttachment> delete = builder.createCriteriaDelete(ProposalCommentAttachment.class);
		Root<ProposalCommentAttachment> root = delete.from(ProposalCommentAttachment.class);
		delete.where(root.get("proposalCommentId").in(proposalCommentIds));
		session.createQuery(delete).executeUpdate();	
	}

	@Override
	public List<Integer> getAllProposalComment(Integer proposalId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> criteria = builder.createQuery(Integer.class);
			Root<ProposalComment> proposalRoot = criteria.from(ProposalComment.class);
			criteria.where(builder.equal(proposalRoot.get("proposalId"), proposalId));
			criteria.select(proposalRoot.get("proposalCommentId"));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<ProposalPersonAttachment> fetchProposalPersonAttachmentWithLastVersion(Set<Integer> proposalPersonId) {
			ArrayList<Integer> proposalPersonIds = new ArrayList<>(proposalPersonId);
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder query = new StringBuilder("SELECT att FROM ProposalPersonAttachment att ");
			query.append(" WHERE (att.documentId, att.versionNumber) IN ");
			query.append("(SELECT DISTINCT attach.documentId, MAX(attach.versionNumber) ");
			query.append("FROM ProposalPersonAttachment attach WHERE attach.proposalPersonId IN :proposalPersonId ");
			query.append("GROUP BY attach.documentId) ");
			query.append("AND att.proposalPersonId IN :proposalPersonId");
			List<ProposalPersonAttachment> dataList = session.createQuery(query.toString())
					.setParameter("proposalPersonId", proposalPersonId).getResultList();
			if (dataList == null || dataList.isEmpty())
				return Collections.emptyList();
			return dataList;
	}
}
