package com.polus.integration.award.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.award.dto.AwardDTO;
import com.polus.integration.award.pojo.COIIntegrationAward;
import com.polus.integration.award.pojo.COIIntegrationAwardPerson;
import com.polus.integration.award.repository.AwardPersonRepository;
import com.polus.integration.award.repository.AwardRepository;
import com.polus.integration.constant.Constant;
import com.polus.integration.dao.IntegrationDao;
import com.polus.integration.exception.service.MQRouterException;
import com.polus.integration.proposal.dto.DisclosureResponse;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceException;
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

	@Autowired
	private AwardRepository awardRepository;

	@Autowired
	private AwardPersonRepository  projectPersonRepository;

	@Autowired
	private JdbcTemplate jdbcTemplate;

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

	@Override
	public void postIntegrationProcess(String projectNumber) {
		try {
			awardRepository.COI_SYNC_REMOVE_DEACTIVATED_PROJECTS(Constant.AWARD_MODULE_CODE, projectNumber);
		} catch (Exception e) {
			log.error("Exception in postIntegrationProcess", e.getMessage());
		}
	}

	@Override
	public void saveAward(COIIntegrationAward award) {
		awardRepository.save(award);		
	}

	@Override
	public void saveAwardPerson(COIIntegrationAwardPerson projectPerson) {
		projectPersonRepository.save(projectPerson);
	}

	@Override
	public DisclosureResponse feedAwardDisclosureStatus(String awardNumber, String personId) {
		try {
			log.info("Calling stored procedure COI_INT_PROJECT_PRSN_DISCL_STATUS with awardNumber: {} and personId: {}", awardNumber, personId);

			return jdbcTemplate.execute((Connection conn) -> {
				try (CallableStatement cs = conn.prepareCall("{call COI_INT_PROJECT_PRSN_DISCL_STATUS(?, ?)}")) {
					cs.setString(1, awardNumber);
					cs.setString(2, personId);

					try (ResultSet rset = cs.executeQuery()) {
						if (rset != null && rset.next()) {
							Integer id = rset.getObject("DISCLOSURE_ID") != null ? rset.getInt("DISCLOSURE_ID") : null;
							String status = rset.getString("DISCLOSURE_STATUS");

							log.info("Disclosure ID: {}, Status: {} for awardNumber: {}, personId: {}", id, status, awardNumber, personId);

							if (status != null) {
								return DisclosureResponse.builder().disclosureId(id).disclosureStatus(status).build();
							} else {
								log.warn("No disclosure data found for awardNumber: {} and personId: {}", awardNumber, personId);
								return DisclosureResponse.builder().message("No data found!").build();
							}
						} else {
							log.warn("ResultSet is empty for awardNumber: {} and personId: {}", awardNumber, personId);
							return DisclosureResponse.builder().message("No data found!").build();
						}
					}
				} catch (SQLException ex) {
					log.error("SQL error during procedure execution for awardNumber: {}, personId: {}: {}", awardNumber, personId, ex.getMessage(), ex);
					throw new RuntimeException("A SQL error occurred during the procedure call.", ex);
				}
			});
		} catch (DataAccessException e) {
			log.error("DataAccessException while fetching disclosure status for awardNumber: {}, personId: {}: {}", awardNumber, personId, e.getMessage(), e);
			return DisclosureResponse.builder().error("Database access error occurred. Please try again later.").build();
		} catch (Exception e) {
			log.error("Unexpected error while fetching disclosure details for awardNumber: {}, personId: {}: {}", awardNumber, personId, e.getMessage(), e);
			return DisclosureResponse.builder().error("An unexpected error occurred. Please try again later.").build();
		}
	}

	@Override
	public DisclosureResponse checkAwardDisclosureStatus(String awardNumber) {
		log.info("Fetching disclosure status for awardNumber: {}", awardNumber);

		try {
			Query query = entityManager.createNativeQuery("SELECT COI_INT_PROJECT_DISCL_STATUS(:awardNumber)")
					.setParameter("awardNumber", awardNumber);

			Object result = query.getSingleResult();
			log.info("Result from function COI_INT_PROJECT_DISCL_STATUS for awardNumber {}: {}", awardNumber, result);

			if (result instanceof Number) {
				Integer disclosureSubmitted = ((Number) result).intValue();
				String message = (disclosureSubmitted == 1) ? "Disclosure Approved." : "Disclosure Not Approved.";
				Boolean isSubmitted = (disclosureSubmitted == 1);

				log.info("Award {} - {} - {}", awardNumber, message, isSubmitted);
				return DisclosureResponse.builder().disclosureSubmitted(isSubmitted).message(message).build();
			} else {
				log.warn("Unexpected result type for awardNumber {}: {}", awardNumber, result);
				return DisclosureResponse.builder().error("Unexpected result from the database.").build();
			}

		} catch (PersistenceException e) {
			log.error("Database error for awardNumber {}: {}", awardNumber, e.getMessage(), e);
			return DisclosureResponse.builder().error("Database access error occurred. Please try again later.").build();
		} catch (Exception e) {
			log.error("Error fetching disclosure status for awardNumber {}: {}", awardNumber, e.getMessage(), e);
			return DisclosureResponse.builder().error("An unexpected error occurred. Please try again later.").build();
		}
	}

}
