package com.polus.integration.award.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.award.dto.AwardDTO;
import com.polus.integration.constant.Constant;
import com.polus.integration.dao.IntegrationDao;
import com.polus.integration.exception.service.MQRouterException;
import jakarta.persistence.EntityManager;
import jakarta.persistence.Query;
import lombok.extern.slf4j.Slf4j;

@Transactional
@Service
@Slf4j
public class AwardIntegrationDaoImpl implements AwardIntegrationDao {

	@Autowired
	private EntityManager entityManager;

	@Autowired
	private IntegrationDao integrationDao;

	@Value("${fibi.messageq.queues.awardIntegration}")
	private String awardIntegrationQueue;

	@Override
	public Boolean canUpdateProjectDisclosureFlag(AwardDTO award) {
	    try {
	        String linkedIPs = !award.getLinkedInstProposalNumbers().isEmpty()
	                ? String.join(",", award.getLinkedInstProposalNumbers())
	                : "";
	        Query query = entityManager.createNativeQuery(
	                "SELECT FN_CHK_DISCL_SYNC_FLAG_REQ_IN_AWD(:projectNumber, :statusCode, :sponsorCode, :primeSponsorCode, :attributeValue1, :linkedIPs)")
	                .setParameter("projectNumber", award.getProjectNumber())
	                .setParameter("statusCode", award.getProjectStatusCode())
	                .setParameter("sponsorCode", award.getSponsorCode())
	                .setParameter("primeSponsorCode", award.getPrimeSponsorCode())
	                .setParameter("attributeValue1", award.getAttribute1Value())
	                .setParameter("linkedIPs", linkedIPs);

	        Object result = query.getSingleResult();
	        if (result instanceof Number) {
	            return ((Number) result).intValue() == 1;
	        }
	    } catch (Exception e) {
	        log.error("Exception occurred in canUpdateProjectDisclosureFlag for project: {}", award.getProjectNumber(), e.getMessage());
	        throw new MQRouterException(Constant.ERROR_CODE, "Exception in feed award integration", e, e.getMessage(),
	                awardIntegrationQueue, null, Constant.FIBI_DIRECT_EXCHANGE,
	                Constant.AWARD_MODULE_CODE, Constant.SUB_MODULE_CODE,
	                Constant.AWARD_INTEGRATION_ACTION_TYPE, integrationDao.generateUUID());
	    }
	    return false;
	}

}
