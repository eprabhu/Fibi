package com.polus.fibicomp.applicationid.service;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.pojo.Proposal;

@Transactional
@Service(value = "applicationIdService")
public class ApplicationIdServiceImpl implements ApplicationIdService {

	protected static Logger logger = LogManager.getLogger(ApplicationIdServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "proposalDao")
	private ProposalDao proposalDao;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public String generateApplicationIdProposal(Proposal proposal) {
		String applicationId = "";
		if (proposal.getGrantCallType() != null && (proposal.getGrantCallType().getCategoryCode() == Constants.GRANT_CALL_TYPE_INTERNAL && proposal.getGrantCallId() != null)) {
			GrantCall grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
			if(grantCall != null) {
				Integer count = 0;
				applicationId = generateApplicationIdForInternalProposal(grantCall.getOpeningDate(),
						proposal.getHomeUnitNumber(), grantCall.getFundingSchemeId(), proposal.getActivityTypeCode(), count);
				while (isApplicationIdFound(applicationId)) {
					count = count +1;
					applicationId = generateApplicationIdForInternalProposal(grantCall.getOpeningDate(),
							proposal.getHomeUnitNumber(), grantCall.getFundingSchemeId(), proposal.getActivityTypeCode(), count);
				}	
			}
//		} else if ((proposal.getGrantTypeCode() == Constants.GRANT_CALL_TYPE_EXTERNAL)) {
//			applicationId = generateApplicationIdForExternalProposal();
//		} else if (proposal.getGrantTypeCode() == Constants.GRANT_CALL_TYPE_OTHERS) {
//			applicationId = generateApplicationIdForOtherProposal();
		} else {
			// applicationId = generateApplicationIdForExternalProposal();
			applicationId = getApplicationIdForExternalProposal();
		}
		return applicationId;
	}

	public String generateApplicationIdForInternalProposal(Timestamp openingDate, String homeUnitNumber,
			Integer fundingSchemeId, String activityTypeCode, Integer count) {
		return getFY(openingDate) + "-" + getHomeUnitCode(homeUnitNumber) + "-" + getType(fundingSchemeId, activityTypeCode) + "-" + getSerialNumber(countOfValue(getFY(openingDate), count));
	}

	public String generateApplicationIdForExternalProposal() {
		Integer count = 0;
		String serialNumber = getSerialNumberForExternalProposal(countOfExternalApplication(count));
		while (isApplicationIdFound(serialNumber)) {
			count = count +1;
			serialNumber = getSerialNumberForExternalProposal(countOfExternalApplication(count));
		}
		return serialNumber;
	}

	private boolean isApplicationIdFound(String serialNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		@SuppressWarnings("unchecked")
		Query<BigInteger> value = session.createSQLQuery("SELECT COUNT(*) FROM EPS_PROPOSAL WHERE APPLICATION_ID = :application_id");
		value.setString("application_id", serialNumber);
		if (value.getSingleResult().intValue() > 0) {
			return true;
		} else {
			return false;
		}
	}

	public String generateApplicationIdForOtherProposal() {
		return getSerialNumber(countOfOtherTypeApplication());
	}

	private String getFY(Timestamp openingDate) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(openingDate);
		int month = calendar.get(Calendar.MONTH);
		int year = calendar.get(Calendar.YEAR);
		if (month < 3) {
			year = year - 1;
		}
		return Integer.toString(year).substring(2, 4);
	}

	private String getHomeUnitCode(String homeUnitNumber) {
		return commonDao.getUnitAcronym(homeUnitNumber);
	}

	private Integer countOfValue(String partialApplicationId, Integer count) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		@SuppressWarnings("unchecked")
		Query<BigInteger> value = session.createSQLQuery("SELECT count(*) FROM EPS_PROPOSAL where APPLICATION_ID like '" + partialApplicationId + "%'");
		return (value.getSingleResult().intValue()) + 1 + count;
	}

	private String getType(Integer fundingSchemeId, String activityTypeCode) {
		String type = null;
		if (fundingSchemeId != null) {
			if (fundingSchemeId.equals(Constants.RCBF_FUNDING_SCHEME_STATUS_CODE) || activityTypeCode.equals(Constants.RCBF_PROPOSAL)) {
				type = "RCB";
			} else {
				type = "SMU";
			}
		} else {
			type = "SMU";
		}
		return type;
	}

	private String getSerialNumber(Integer countValue) {
		String serialNo = null;
		if (countValue < 10) {
			serialNo = "00" + countValue;
		} else if (countValue >= 10 && countValue < 100) {
			serialNo = "0" + countValue;
		} else {
			serialNo = "" + countValue;
		}
		return serialNo;
	}

	private String getSerialNumberForExternalProposal(Integer countValue) {
		String serialNo = null;
		if (countValue < 10) {
			serialNo = "00000" + countValue;
		} else if (countValue >= 10 && countValue < 100) {
			serialNo = "0000" + countValue;
		} else if(countValue >= 100 && countValue < 1000) {
			serialNo = "000" + countValue;
		} else if(countValue >= 1000 && countValue < 10000) {
			serialNo = "00" + countValue;
		} else if(countValue >= 10000 && countValue < 100000) {
			serialNo = "0" + countValue;
		} else {
			serialNo = "" + countValue;
		}
		return serialNo;
	}

	private Integer countOfExternalApplication(Integer count) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		@SuppressWarnings("unchecked")
		Query<BigInteger> value = session.createSQLQuery("SELECT COUNT(*) FROM EPS_PROPOSAL WHERE GRANT_TYPE_CODE IN (2,3) and APPLICATION_ID IS NOT NULL");
		return (value.getSingleResult().intValue()) + 1 + count;
	}

	private Integer countOfOtherTypeApplication() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		@SuppressWarnings("unchecked")
		Query<BigInteger> value = session.createSQLQuery("SELECT COUNT(*) FROM EPS_PROPOSAL WHERE GRANT_TYPE_CODE = 3 and APPLICATION_ID IS NOT NULL");
		return (value.getSingleResult().intValue()) + 1;
	}

	public String getApplicationIdForExternalProposal() {
		Integer count = 0;
		String serialNumber = getSerialNumberForExternalProposal(fetchApplicationIdBasedOnCriteria(count));
		logger.info("serialNumber : {}", serialNumber);
		while (isApplicationIdFound(serialNumber)) {
			count = count + 1;
			serialNumber = getSerialNumberForExternalProposal(fetchApplicationIdBasedOnCriteria(count));
		}
		return serialNumber;
	}

	@SuppressWarnings("unchecked")
	private Integer fetchApplicationIdBasedOnCriteria(Integer count) {
		Integer applicationId = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query value = session.createSQLQuery("SELECT APPLICATION_ID FROM EPS_PROPOSAL WHERE GRANT_TYPE_CODE IN (2,3) and APPLICATION_ID IS NOT NULL");
		List<String> applicationIds = value.getResultList();
		logger.info("applicationIds : {}", applicationIds);
		if (applicationIds != null && !applicationIds.isEmpty()) {
			List<Integer> parsedApplicationIds = applicationIds.stream().map(Integer::parseInt).collect(Collectors.toList());
			logger.info("parsedApplicationIds : {}", parsedApplicationIds);
			Integer maxApplicationId = Collections.max(parsedApplicationIds);
			logger.info("maxApplicationId : {}", maxApplicationId);
			applicationId = maxApplicationId + 1 + count;
			logger.info("applicationId : {}", applicationId);
		}
		return applicationId;
	}
}
