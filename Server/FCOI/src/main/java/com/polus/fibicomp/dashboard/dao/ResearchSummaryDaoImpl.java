package com.polus.fibicomp.dashboard.dao;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.ParameterMode;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.vo.ProposalView;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.pojo.QuickLink;
import com.polus.fibicomp.dashboard.pojo.UserSelectedWidget;
import com.polus.fibicomp.dashboard.pojo.WidgetLookup;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.utils.DashBoardQueries;
import com.polus.fibicomp.view.AwardView;
import com.polus.fibicomp.view.ExpenditureVolume;
import com.polus.fibicomp.view.ResearchSummaryPieChart;
import com.polus.fibicomp.view.ResearchSummaryView;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "researchSummaryDao")
@PropertySource("classpath:application.properties")
public class ResearchSummaryDaoImpl implements ResearchSummaryDao {

	protected static Logger logger =  LogManager.getLogger(ResearchSummaryDaoImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	private static final String UNIT_NUMBER = "unitNumber";
	private static final String PERSON_ID = "personId";
	private static final String SPONSOR_CODE = "sponsorCode";

	@SuppressWarnings({ "unchecked", "unused" })
	private List<Unit> getUnitsWithRighs(String personId) {
		List<Unit> units = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query unitsWithRights = null;
		unitsWithRights = session.createSQLQuery(DashBoardQueries.UNIT_WITH_RIGHT_OF_PERSON);
		unitsWithRights.setParameter(PERSON_ID, personId);
		List<Object[]> unitDetails = unitsWithRights.getResultList();
		for (Object[] unitDetail : unitDetails) {
			Unit unit = new Unit();
			unit.setUnitNumber(unitDetail[0].toString());
			unit.setUnitName(unitDetail[1].toString());
			units.add(unit);
		}
		return units;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ExpenditureVolume> getExpenditureVolumeChart(String personId, String unitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query expenditureVolume = null;
		if (oracledb.equals("Y")) {
			if (unitNumber != null) {
				expenditureVolume = session.createSQLQuery(DashBoardQueries.EXPENDITURE_VOLUME_WITH_UNIT_ORACLE);
				expenditureVolume.setParameter(UNIT_NUMBER, unitNumber);
			} else {
				expenditureVolume = session.createSQLQuery(DashBoardQueries.EXPENDITURE_VOLUME_ORACLE);
				expenditureVolume.setParameter(PERSON_ID, personId);
			}
		} else {
			if (unitNumber != null) {
				expenditureVolume = session.createSQLQuery(DashBoardQueries.EXPENDITURE_VOLUME_WITH_UNIT);
				expenditureVolume.setParameter(UNIT_NUMBER, unitNumber);
			} else {
				expenditureVolume = session.createSQLQuery(DashBoardQueries.EXPENDITURE_VOLUME);
				expenditureVolume.setParameter(PERSON_ID, personId);
			}
		}
		return expenditureVolume.getResultList();
	}

	@SuppressWarnings({ "unchecked"})
	@Override
	public List<ResearchSummaryView> getSummaryTable(String personId, String unitNumber) {
		List<ResearchSummaryView> summaryTable = new ArrayList<>();
		List<ResearchSummaryView> subPropCount = null;
		List<ResearchSummaryView> inPropCount = null;
		List<ResearchSummaryView> activeAwardsCount = null;
		Query submittedProposal = null;
		Query inprogressProposal = null;
		Query activeAwards = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			if (unitNumber != null) {
				submittedProposal = session.createSQLQuery(DashBoardQueries.APPROVAL_INPROGRESS_PROPOSAL_WITH_UNIT);
				submittedProposal.setParameter(UNIT_NUMBER, unitNumber);
			} 
			else {
				submittedProposal = session.createSQLQuery(DashBoardQueries.APPROVAL_INPROGRESS_PROPOSAL);
				submittedProposal.setParameter(PERSON_ID, personId);
			}
		subPropCount = submittedProposal.getResultList();
		if (subPropCount != null && !subPropCount.isEmpty()) {
			summaryTable.addAll(subPropCount);
		}
			if (unitNumber != null) {
				inprogressProposal = session.createSQLQuery(DashBoardQueries.INPROGRESS_PROPOSAL_WITH_UNIT);
				inprogressProposal.setParameter(UNIT_NUMBER, unitNumber);
			} else {
				inprogressProposal = session.createSQLQuery(DashBoardQueries.INPROGRESS_PROPOSAL);
				inprogressProposal.setParameter(PERSON_ID, personId);
			}
		inPropCount = inprogressProposal.getResultList();
		if (inPropCount != null && !inPropCount.isEmpty()) {
			summaryTable.addAll(inPropCount);
		}
		if (unitNumber != null) {		
			activeAwards = session.createSQLQuery(DashBoardQueries.AWARDED_PROPOSAL_WITH_UNIT);		
			activeAwards.setParameter(UNIT_NUMBER, unitNumber);		
		} else {		
			activeAwards = session.createSQLQuery(DashBoardQueries.AWARDED_PROPOSAL_WITHOUT_UNIT);		
			activeAwards.setParameter(PERSON_ID, personId);		
		}		
		activeAwardsCount = activeAwards.getResultList();		
		if (activeAwardsCount != null && !activeAwardsCount.isEmpty()) {		
			summaryTable.addAll(activeAwardsCount);		
		}		
		return summaryTable;
	}

	@SuppressWarnings({ "unchecked"})
	@Override
	public List<ResearchSummaryPieChart> getSummaryAwardPieChart(String personId, String unitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = null;
		if (unitNumber != null) {
			query = session.createSQLQuery(DashBoardQueries.SUMMARY_AWARD_PIE_CHART_WITH_UNIT);
			query.setParameter(UNIT_NUMBER, unitNumber);		
			}else {
				query = session.createSQLQuery(DashBoardQueries.SUMMARY_AWARD_PIE_CHART);
				query.setParameter(PERSON_ID, personId);
			}
		return (List<ResearchSummaryPieChart>) query.getResultList();
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public List<ResearchSummaryPieChart> getSummaryProposalPieChart(String personId, String unitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = null;
		if (unitNumber != null) {
		query = session.createSQLQuery(DashBoardQueries.SUMMARY_INPROGRESS_PIE_CHART_WITH_UNIT);
		query.setParameter(UNIT_NUMBER, unitNumber);		
		}else {
			query = session.createSQLQuery(DashBoardQueries.SUMMARY_INPROGRESS_PIE_CHART);
			query.setParameter(PERSON_ID, personId);
		}
		return (List<ResearchSummaryPieChart>) query.getResultList();
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public List<ResearchSummaryPieChart> getSummaryInProgressProposalDonutChart(String personId, String unitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = null;
		if (unitNumber != null) {
			query = session.createSQLQuery(DashBoardQueries.SUMMARY_INPROGRESS_DONET_WITH_UNIT);
			query.setParameter(UNIT_NUMBER, unitNumber);
		} else {
			query = session.createSQLQuery(DashBoardQueries.SUMMARY_INPROGRESS_DONET);
			query.setParameter(PERSON_ID, personId);
		}
		return (List<ResearchSummaryPieChart>) query.getResultList();
	}

	@SuppressWarnings({ "unchecked"})
	@Override
	public List<ResearchSummaryPieChart> getSummaryAwardedProposalDonutChart(String personId, String unitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = null;
		if (unitNumber != null) {
			query = session.createSQLQuery(DashBoardQueries.SUMMARY_AWARD_DONET_WITH_UNIT);
			query.setParameter(UNIT_NUMBER, unitNumber);
		} else {
			query = session.createSQLQuery(DashBoardQueries.SUMMARY_AWARD_DONET);
			query.setParameter(PERSON_ID, personId);
		}
		return (List<ResearchSummaryPieChart>) query.getResultList();
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public DashBoardProfile getProposalsInProgress(String personId, String unitNumber) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<ProposalView> inProgressProposals = new ArrayList<>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query progressProposalList = null;
			if (unitNumber != null) {
				progressProposalList = session.createSQLQuery(DashBoardQueries.INPROGRESS_PROPOSAL_LIST_WITH_UNIT);
				progressProposalList.setParameter(UNIT_NUMBER, unitNumber);
			}else {
				progressProposalList = session.createSQLQuery(DashBoardQueries.INPROGRESS_PROPOSAL_LIST);
				progressProposalList.setParameter(PERSON_ID, personId);
			}
			List<Object[]> proposals = progressProposalList.getResultList();
			for (Object[] proposal : proposals) {
				ProposalView proposalView = new ProposalView();
				proposalView.setProposalNumber(proposal[0].toString());
				proposalView.setTitle(proposal[1].toString());
				proposalView.setSponsor((proposal[2] != null ? proposal[2].toString() : null));
				if (proposal[3] != null) {
					proposalView.setTotalCost(proposal[3].toString());
				} else {
					proposalView.setTotalCost("0.00");
				}
				proposalView.setFullName((proposal[4] != null ? proposal[4].toString() : null));
				proposalView.setLeadUnit((proposal[5] != null ? proposal[5].toString() : null));				
				Object deadLineObj = proposal[6];
				if (deadLineObj != null) {
					Timestamp deadLineDate = (Timestamp) deadLineObj;
					proposalView.setDeadLinedate(deadLineDate);
				}
				inProgressProposals.add(proposalView);
			}
			dashBoardProfile.setProposalViews(inProgressProposals);
		} catch (Exception e) {
			logger.error("Error in method getProposalsInProgress");
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public DashBoardProfile getApprovalInProgressProposals(String personId, String unitNumber) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<ProposalView> submittedProposals = new ArrayList<>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query subproposalList = null;
			if (unitNumber != null) {
				subproposalList = session.createSQLQuery(DashBoardQueries.APPROVAL_INPROGRESS_PROPOSAL_LIST_WITH_UNIT);
				subproposalList.setParameter(UNIT_NUMBER, unitNumber);
			}else {
				subproposalList = session.createSQLQuery(DashBoardQueries.APPROVAL_INPROGRESS_PROPOSAL_LIST);
				subproposalList.setParameter(PERSON_ID, personId);
			}
			List<Object[]> subProposals = subproposalList.getResultList();
			for (Object[] proposal : subProposals) {
				ProposalView proposalView = new ProposalView();
				proposalView.setProposalNumber(proposal[0].toString());
				proposalView.setTitle(proposal[1].toString());
				proposalView.setSponsor(proposal[2].toString());
				if (proposal[3] != null) {
					proposalView.setTotalCost(proposal[3].toString());
				} else {
					proposalView.setTotalCost("0.00");
				}
				if (proposal[4] != null) {
					proposalView.setFullName(proposal[4].toString());
				}
				proposalView.setLeadUnit(proposal[5] == null ? null : proposal[5].toString());
				proposalView.setCertified(true);
				Object deadLineObj = proposal[6];
				if (deadLineObj != null) {
					Timestamp deadLineDate = (Timestamp) deadLineObj;
					proposalView.setDeadLinedate(deadLineDate);
				}
				submittedProposals.add(proposalView);
			}
			dashBoardProfile.setProposalViews(submittedProposals);
		} catch (Exception e) {
			logger.error("Error in method getApprovalInProgressProposals");
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public String getInProgressProposalsBySponsorExpanded(String personId, String sponsorCode, String unitNumber) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<ProposalView> inProgressProposal = new ArrayList<>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query proposalQuery = null;
			if (unitNumber != null) {
				proposalQuery = session.createSQLQuery(DashBoardQueries.PROPOSAL_DONUT_WITH_SPONSOR_AND_UNIT);
				proposalQuery.setParameter(UNIT_NUMBER, unitNumber);
				proposalQuery.setParameter(SPONSOR_CODE, sponsorCode);
			}else {
				proposalQuery = session.createSQLQuery(DashBoardQueries.PROPOSAL_DONUT_WITH_SPONSOR);
				proposalQuery.setParameter(PERSON_ID, personId);
				proposalQuery.setParameter(SPONSOR_CODE, sponsorCode);
			}
			inProgressProposal = proposalQuery.getResultList();
			dashBoardProfile.setProposalViews(inProgressProposal);
		} catch (Exception e) {
			logger.error("Error in method getInProgressProposalsBySponsorExpanded");
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public String getAwardedProposalsBySponsorExpanded(String personId, String sponsorCode, String unitNumber) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<ProposalView> awardedProposal = new ArrayList<>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query proposalQuery = null;
			if(unitNumber != null) {
				proposalQuery = session.createSQLQuery(DashBoardQueries.AWARD_DONUT_WITH_SPONSOR_AND_UNIT);
				proposalQuery.setParameter(UNIT_NUMBER, unitNumber);
				proposalQuery.setParameter(SPONSOR_CODE, sponsorCode);
			}else {
				proposalQuery = session.createSQLQuery(DashBoardQueries.AWARD_DONUT_WITH_SPONSOR);
				proposalQuery.setParameter(SPONSOR_CODE, sponsorCode);
				proposalQuery.setParameter(PERSON_ID, personId);
			}
			awardedProposal = proposalQuery.getResultList();
			dashBoardProfile.setProposalViews(awardedProposal);
		} catch (Exception e) {
			logger.error("Error in method getAwardedProposalsBySponsorExpanded");
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public String getAwardBySponsorTypes(String personId, String sponsorCode, String unitNumber) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<AwardView> awardBySponsorTypes = new ArrayList<>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query awardList = null;
			if (unitNumber != null) {
				awardList = session.createSQLQuery(DashBoardQueries.AWARD_BY_SPONSOR_WITH_UNIT);
				awardList.setParameter(SPONSOR_CODE, sponsorCode);
				awardList.setParameter(UNIT_NUMBER, unitNumber);
			}else {
				awardList = session.createSQLQuery(DashBoardQueries.AWARD_BY_SPONSOR);
				awardList.setParameter(SPONSOR_CODE, sponsorCode);
				awardList.setParameter(PERSON_ID, personId);
			}
			awardBySponsorTypes = awardList.getResultList();
			dashBoardProfile.setAwardViews(awardBySponsorTypes);
		} catch (Exception e) {
			logger.error("Error in method getPieChartAwardbySponsorTypes");
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public String getProposalBySponsorTypes(String personId, String sponsorCode, String unitNumber) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();		
		List<ProposalView> proposalBySponsorTypes = new ArrayList<>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query proposalList = null;
			if (unitNumber != null) {
				proposalList = session.createSQLQuery(DashBoardQueries.PROPOSAL_BY_SPONSOR_WITH_UNIT);
				proposalList.setParameter(UNIT_NUMBER, unitNumber);
				proposalList.setParameter(SPONSOR_CODE, sponsorCode);
			}else {
				proposalList = session.createSQLQuery(DashBoardQueries.PROPOSAL_BY_SPONSOR);
				proposalList.setParameter(PERSON_ID, personId);
				proposalList.setParameter(SPONSOR_CODE, sponsorCode);
			}
			proposalBySponsorTypes = proposalList.getResultList();
			dashBoardProfile.setProposalViews(proposalBySponsorTypes);
		} catch (Exception e) {
			logger.error("Error in method getPieChartAwardbySponsorTypes");
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}
	
	@SuppressWarnings({ "unchecked" })
	@Override
	public DashBoardProfile getSubmittedProposals(String personId, String unitNumber) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<ProposalView> submittedProposal = new ArrayList<>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query submittedProposalQuery = null;
			if (unitNumber != null) {
				submittedProposalQuery = session.createSQLQuery(DashBoardQueries.DETAILED_AWARDED_PROPOSAL_WITH_UNIT);
				submittedProposalQuery.setParameter(UNIT_NUMBER, unitNumber);
			} else {
				submittedProposalQuery = session.createSQLQuery(DashBoardQueries.DETAILED_AWARDED_PROPOSAL_WITHOUT_UNIT);
				submittedProposalQuery.setParameter(PERSON_ID, personId);
			}
			List<Object[]>submittedProposalView = submittedProposalQuery.getResultList();
			for (Object[] submittedProposallistView : submittedProposalView) {
				ProposalView proposalView = new ProposalView();
				proposalView.setProposalNumber(submittedProposallistView[0].toString());
				proposalView.setTitle(submittedProposallistView[1].toString());
				proposalView.setSponsor(submittedProposallistView[2].toString());
				
				if (submittedProposallistView[3] != null) {
					proposalView.setTotalCost(submittedProposallistView[3].toString());
				} else {
					proposalView.setTotalCost("0.00");
				}
				if (submittedProposallistView[4] != null) {
					proposalView.setFullName(submittedProposallistView[4].toString());
				}
				proposalView.setHomeUnit(submittedProposallistView[5].toString());

				Object deadLineObj = submittedProposallistView[6];
				if (deadLineObj != null) {
					Timestamp deadLineDate = (Timestamp) deadLineObj;
					proposalView.setDeadLinedate(deadLineDate);
				}
				submittedProposal.add(proposalView);
			}
			dashBoardProfile.setProposalViews(submittedProposal);

		} catch (Exception e) {
			logger.error("Error in method getSubmittedProposals");
			e.printStackTrace();
		}
		return dashBoardProfile;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getInprogressProposalsForDownload(String personId, String unitNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			javax.persistence.Query progressProposalList = null;
			if (unitNumber != null) {
				progressProposalList = session.createSQLQuery(DashBoardQueries.DOWNLOAD_INPROGRESS_PROPOSAL_LIST_WITH_UNIT);
				progressProposalList.setParameter(UNIT_NUMBER, unitNumber);
			} else {
				progressProposalList = session.createSQLQuery(DashBoardQueries.DOWNLOAD_INPROGRESS_PROPOSAL_LIST);
				progressProposalList.setParameter(PERSON_ID, personId);
			}
			return progressProposalList.getResultList();
		} catch (Exception e) {
			logger.error("Error in method getInprogressProposalsForDownload");
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getSubmittedProposalsForDownload(String personId, String unitNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			javax.persistence.Query submittedList = null;
			if (unitNumber != null) {
				submittedList = session.createSQLQuery(DashBoardQueries.DOWNLOAD_DETAILED_AWARDED_PROPOSAL_WITH_UNIT);
				submittedList.setParameter(UNIT_NUMBER, unitNumber);
			} else {
				submittedList = session.createSQLQuery(DashBoardQueries.DOWNLOAD_DETAILED_AWARDED_PROPOSAL_WITHOUT_UNIT);
				submittedList.setParameter(PERSON_ID, personId);
			}
			return submittedList.getResultList();
		} catch (Exception e) {
			logger.error("Error in method getSubmittedProposalsForDownload");
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getInProgressProposalsBySponsorForDownload(String personId, String sponsorCode, String unitNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			javax.persistence.Query proposalQuery = null;
			if (unitNumber != null) {
				proposalQuery = session.createSQLQuery(DashBoardQueries.DOWNLOAD_PROPOSAL_DONUT_WITH_SPONSOR_AND_UNIT);
				proposalQuery.setParameter(UNIT_NUMBER, unitNumber);
				proposalQuery.setParameter(SPONSOR_CODE, sponsorCode);
			}else {
				proposalQuery = session.createSQLQuery(DashBoardQueries.DOWNLOAD_PROPOSAL_DONUT_WITH_SPONSOR);
				proposalQuery.setParameter(PERSON_ID, personId);
				proposalQuery.setParameter(SPONSOR_CODE, sponsorCode);
			}
			return proposalQuery.getResultList();
		} catch (Exception e) {
			logger.error("Error in method getInProgressProposalsBySponsorForDownload");
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getAwardedProposalsBySponsorForDownload(String personId, String sponsorCode, String unitNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			javax.persistence.Query proposalQuery = null;
			if(unitNumber != null) {
				proposalQuery = session.createSQLQuery(DashBoardQueries.DOWNLOAD_AWARD_DONUT_WITH_SPONSOR_AND_UNIT);
				proposalQuery.setParameter(UNIT_NUMBER, unitNumber);
				proposalQuery.setParameter(SPONSOR_CODE, sponsorCode);
			}else {
				proposalQuery = session.createSQLQuery(DashBoardQueries.DOWNLOAD_AWARD_DONUT_WITH_SPONSOR);
				proposalQuery.setParameter(SPONSOR_CODE, sponsorCode);
				proposalQuery.setParameter(PERSON_ID, personId);
			}
			return proposalQuery.getResultList();
		} catch (Exception e) {
			logger.error("Error in method getAwardedProposalsBySponsorForDownload");
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getAwardBySponsorTypesForDownload(String personId, String sponsorCode, String unitNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			javax.persistence.Query awardList = null;
			if (unitNumber != null) {
				awardList = session.createSQLQuery(DashBoardQueries.DOWNLOAD_AWARD_BY_SPONSOR_WITH_UNIT);
				awardList.setParameter(SPONSOR_CODE, sponsorCode);
				awardList.setParameter(UNIT_NUMBER, unitNumber);
			}else {
				awardList = session.createSQLQuery(DashBoardQueries.DOWNLOAD_AWARD_BY_SPONSOR);
				awardList.setParameter(SPONSOR_CODE, sponsorCode);
				awardList.setParameter(PERSON_ID, personId);
			}
			return awardList.getResultList();
		} catch (Exception e) {
			logger.error("Error in method getAwardBySponsorTypesForDownload");
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getProposalBySponsorTypesForDownload(String personId, String sponsorCode, String unitNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			javax.persistence.Query proposalList = null;
			if (unitNumber != null) {
				proposalList = session.createSQLQuery(DashBoardQueries.PROPOSAL_BY_SPONSOR_WITH_UNIT);
				proposalList.setParameter(UNIT_NUMBER, unitNumber);
				proposalList.setParameter(SPONSOR_CODE, sponsorCode);
			}else {
				proposalList = session.createSQLQuery(DashBoardQueries.PROPOSAL_BY_SPONSOR);
				proposalList.setParameter(PERSON_ID, personId);
				proposalList.setParameter(SPONSOR_CODE, sponsorCode);
			}
			return proposalList.getResultList();
		} catch (Exception e) {
			logger.error("Error in method getProposalBySponsorTypesForDownload");
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getApprovalInProgressProposalsForDownlaod(String personId, String unitNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			javax.persistence.Query subproposalList = null;
			if (unitNumber != null) {
				subproposalList = session.createSQLQuery(DashBoardQueries.DOWNLOAD_APPROVAL_INPROGRESS_PROPOSAL_LIST_WITH_UNIT);
				subproposalList.setParameter(UNIT_NUMBER, unitNumber);
			}else {
				subproposalList = session.createSQLQuery(DashBoardQueries.DOWNLOAD_APPROVAL_INPROGRESS_PROPOSAL_LIST);
				subproposalList.setParameter(PERSON_ID, personId);
			}
			return subproposalList.getResultList();
		} catch (Exception e) {
			logger.error("Error in method getApprovalInProgressProposals");
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@Override
	public List<QuickLink> getAllQuickLinksOrEvents() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<QuickLink> query = builder.createQuery(QuickLink.class);
		Root<QuickLink> quickLinkRoot = query.from(QuickLink.class);
		Predicate startDate = builder.lessThan(quickLinkRoot.get("startDate"), commonDao.getCurrentTimestamp());
		Predicate endDate = builder.greaterThan(quickLinkRoot.get("endDate"), commonDao.getCurrentTimestamp());
		Predicate withDate = builder.and(startDate, endDate);
		Predicate isNullStartDate = quickLinkRoot.get("startDate").isNull();
		Predicate isNullEndDate = quickLinkRoot.get("endDate").isNull();
		Predicate startDateOnly = builder.and(startDate, isNullEndDate);
		Predicate endDateOnly = builder.and(isNullStartDate, endDate);
		Predicate withOutDate = builder.and(isNullStartDate, isNullEndDate);
		query.where(builder.or(withDate, startDateOnly,endDateOnly,withOutDate));
		query.orderBy(builder.asc(quickLinkRoot.get("sortOrder")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<WidgetLookup> getAllWidgetLookups() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WidgetLookup> query = builder.createQuery(WidgetLookup.class);
		Root<WidgetLookup> widgetRoot = query.from(WidgetLookup.class);
		query.where(builder.equal(widgetRoot.get("active"), Boolean.TRUE));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<UserSelectedWidget> getUserSelectedWidgets(String loginPersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<UserSelectedWidget> query = builder.createQuery(UserSelectedWidget.class);
		Root<UserSelectedWidget> widgetRoot = query.from(UserSelectedWidget.class);
		Predicate personId = builder.equal(widgetRoot.get("personId"), loginPersonId);
		Predicate active = builder.equal(widgetRoot.get("widgetLookup").get("active"), Boolean.TRUE);
		query.where(builder.and(personId, active));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void saveUserSelectedWidget(UserSelectedWidget userSelectedWidget) {
		hibernateTemplate.saveOrUpdate(userSelectedWidget);
	}

	@Override
	public void deleteUserSelectedWidget(Integer selectedWidgetId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<UserSelectedWidget> delete = builder.createCriteriaDelete(UserSelectedWidget.class);
		Root<UserSelectedWidget> root = delete.from(UserSelectedWidget.class);
		delete.where(builder.equal(root.get("selectedWidgetId"), selectedWidgetId));
		session.createQuery(delete).executeUpdate();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getResearchSummaryDatasByWidget(CommonVO commonVO) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			return session.createStoredProcedureCall("GET_RESEARCH_SUMMARY_DETAILS").
					registerStoredProcedureParameter("AV_WIDGET", String.class, ParameterMode.IN).setParameter("AV_WIDGET", commonVO.getTabName())
					.registerStoredProcedureParameter("AV_PERSON_ID", String.class, ParameterMode.IN).setParameter("AV_PERSON_ID", commonVO.getPersonId())
					.registerStoredProcedureParameter("AV_LEVEL", Integer.class, ParameterMode.IN).setParameter("AV_LEVEL", commonVO.getLevel() != null ? commonVO.getLevel() : 1)
					.registerStoredProcedureParameter("AV_UNIT_NUMBER", String.class, ParameterMode.IN).setParameter("AV_UNIT_NUMBER", commonVO.getUnitNumber() != null ? commonVO.getUnitNumber() : "")
					.registerStoredProcedureParameter("AV_SPONSOR_CODE", String.class, ParameterMode.IN).setParameter("AV_SPONSOR_CODE", commonVO.getSponsorCodes() != null ? String.join(",", commonVO.getSponsorCodes()) : "")
					.registerStoredProcedureParameter("AV_SPONSOR_GROUP_ID", String.class, ParameterMode.IN).setParameter("AV_SPONSOR_GROUP_ID", commonVO.getSponsorGroupId() != null ? commonVO.getSponsorGroupId() : "")
					.registerStoredProcedureParameter("AV_START_DATE", String.class, ParameterMode.IN).setParameter("AV_START_DATE", commonVO.getStartDate() != null ? commonVO.getStartDate() : "")
					.registerStoredProcedureParameter("AV_END_DATE", String.class, ParameterMode.IN).setParameter("AV_END_DATE", commonVO.getEndDate() != null ? commonVO.getEndDate() : "")
					.registerStoredProcedureParameter("AV_DESCENT_FLAG", String.class, ParameterMode.IN).setParameter("AV_DESCENT_FLAG", commonVO.getDescentFlag() != null ? commonVO.getDescentFlag() : "N")
					.getResultList();
		} catch (Exception e) {
			throw new ApplicationException("getResearchSummaryDatasByWidget", e, Constants.JAVA_ERROR);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getDetailedViewOfWidget(CommonVO commonVO ) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			return session.createStoredProcedureCall("GET_WIDGET_DATA_IN_DETAIL").
					registerStoredProcedureParameter("AV_WIDGET", String.class, ParameterMode.IN).setParameter("AV_WIDGET", commonVO.getTabName())
					.registerStoredProcedureParameter("AV_PERSON_ID", String.class, ParameterMode.IN).setParameter("AV_PERSON_ID", commonVO.getPersonId())
					.registerStoredProcedureParameter("AV_SEARCH", String.class, ParameterMode.IN).setParameter("AV_SEARCH", commonVO.getProperty1() != null ? commonVO.getProperty1() : "")
					.registerStoredProcedureParameter("AV_UNIT_NUMBER", String.class, ParameterMode.IN).setParameter("AV_UNIT_NUMBER", commonVO.getUnitNumber() != null ? commonVO.getUnitNumber() : "")
					.registerStoredProcedureParameter("AV_SPONSOR_CODE", String.class, ParameterMode.IN).setParameter("AV_SPONSOR_CODE", commonVO.getSponsorCodes() != null ? String.join(",", commonVO.getSponsorCodes()) : "")
					.registerStoredProcedureParameter("AV_SPONSOR_GROUP_ID", String.class, ParameterMode.IN).setParameter("AV_SPONSOR_GROUP_ID", commonVO.getSponsorGroupId() != null ? commonVO.getSponsorGroupId() : "")
					.registerStoredProcedureParameter("AV_START_DATE", String.class, ParameterMode.IN).setParameter("AV_START_DATE", commonVO.getStartDate() != null ? commonVO.getStartDate() : "")
					.registerStoredProcedureParameter("AV_END_DATE", String.class, ParameterMode.IN).setParameter("AV_END_DATE", commonVO.getEndDate() != null ? commonVO.getEndDate() : "")
					.registerStoredProcedureParameter("AV_DESCENT_FLAG", String.class, ParameterMode.IN).setParameter("AV_DESCENT_FLAG", commonVO.getDescentFlag() != null ? commonVO.getDescentFlag() : "N")
					.getResultList();
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public UserSelectedWidget checkIfWidgetAlreadyExist(Integer widgetId, String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<UserSelectedWidget> query = builder.createQuery(UserSelectedWidget.class);
			Root<UserSelectedWidget> widgetRoot = query.from(UserSelectedWidget.class);
			Predicate predicatePersonId = builder.equal(widgetRoot.get("personId"), personId);
			Predicate predicateWidgetId = builder.equal(widgetRoot.get("widgetId"), widgetId);
			query.where(builder.and(predicatePersonId, predicateWidgetId));
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			return null;
		}	
	}

	@Override
	public String getDashBoardResearchSummary(String personId, Boolean isAdmin) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<Object[]> agreementView = getAgreementSummaryTable(personId, isAdmin);
		dashBoardProfile.setAgreementView(agreementView);
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@SuppressWarnings({ "unchecked" })
	private List<Object[]> getAgreementSummaryTable(String personId, Boolean isAdmin) {
		Query summaryView = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if (Boolean.TRUE.equals(isAdmin)) {
			summaryView = session.createSQLQuery(DashBoardQueries.AGREEMENT_SUMMARY_FOR_ADMIN);
			summaryView.setParameter("PERSON_ID", personId);
			
		} else {
			summaryView = session.createSQLQuery(DashBoardQueries.AGREEMENT_SUMMARY_FOR_NON_ADMIN);
			summaryView.setParameter("PERSON_ID", personId);
		}
		return summaryView.getResultList();
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<Unit> getUnitWithRights(String personId) {
		List<Unit> units = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query unitsWithRights = null;
		unitsWithRights = session.createSQLQuery(DashBoardQueries.UNIT_WITH_RIGHT_OF_PERSON);
		unitsWithRights.setParameter(PERSON_ID, personId);
		List<Object[]> unitDetails = unitsWithRights.getResultList();
		for (Object[] unitDetail : unitDetails) {
			Unit unit = new Unit();
			unit.setUnitNumber(unitDetail[0].toString());
			unit.setUnitName(unitDetail[1].toString());
			unit.setParentUnitNumber(unitDetail[2] != null ? unitDetail[2].toString() : null);
			units.add(unit);
		}
		return units;
	}

}
