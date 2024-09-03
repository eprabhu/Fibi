package com.polus.integration.feedentity.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.feedentity.dto.EntityDTO;
import com.polus.integration.feedentity.dto.EntityResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Transactional
@Service
public class EntityIntegrationDao {

	@Autowired
	private JdbcTemplate jdbcTemplate;

	public List<EntityDTO> getEntityDetails(Integer entityId) {
		List<EntityDTO> dtos = new ArrayList<>();
		try {
			log.info("Calling stored procedure COI_INT_GET_ENTITY_DETAILS with entityId: {}", entityId);

			dtos = jdbcTemplate.execute((Connection conn) -> {
				try (CallableStatement cs = conn.prepareCall("{call COI_INT_GET_ENTITY_DETAILS(?)}")) {
					if (entityId != null) {
						cs.setInt(1, entityId);
					} else {
						cs.setNull(1, java.sql.Types.INTEGER);
					}

					try (ResultSet rset = cs.executeQuery()) {
						List<EntityDTO> results = new ArrayList<>();
						while (rset.next()) {
							results.add(EntityDTO.builder()
								.certifiedEmail(rset.getString("CERTIFIED_EMAIL"))
								.sponsorCity(rset.getString("SPONSOR_CITY"))
								.sponsorCountryCode(rset.getString("SPONSOR_COUNTRY_CODE"))
								.sponsorPostCode(rset.getString("SPONSOR_POST_CODE"))
								.sponsorAddressLine1(rset.getString("SPONSOR_ADDRESS_LINE_1"))
								.sponsorAddressLine2(rset.getString("SPONSOR_ADDRESS_LINE_2"))
								.sponsorState(rset.getString("SPONSOR_STATE"))
								.orgCity(rset.getString("ORG_CITY"))
								.orgCountryCode(rset.getString("ORG_COUNTRY_CODE"))
								.orgPostCode(rset.getString("ORG_POST_CODE"))
								.orgAddressLine1(rset.getString("ORG_ADDRESS_LINE_1"))
								.orgAddressLine2(rset.getString("ORG_ADDRESS_LINE_2"))
								.orgState(rset.getString("ORG_STATE"))
								.primaryName(rset.getString("PRIMARY_NAME"))
								.telephoneNumber(rset.getString("TELEPHONE_NUMBER"))
								.sponsorCode(rset.getString("SPONSOR_CODE"))
								.sponsorTypeCode(rset.getString("SPONSOR_TYPE_CODE"))
								.customerNumber(rset.getString("CUSTOMER_NUMBER"))
								.auditReportSentForFy(rset.getString("AUDIT_REPORT_SENT_FOR_FY"))
								.ueiNumber(rset.getString("UEI_NUMBER"))
								.dodacNumber(rset.getString("DODAC_NUMBER"))
								.dunsNumber(rset.getString("DUNS_NUMBER"))
								.acronym(rset.getString("ACRONYM"))
								.cageNumber(rset.getString("CAGE_NUMBER"))
								.dunningCampaignId(rset.getString("DUNNING_CAMPAIGN_ID"))
								.entityId(rset.getInt("ENTITY_ID"))
								.organizationId(rset.getString("ORGANIZATION_ID"))
								.numberOfEmployees(rset.getInt("NUMBER_OF_EMPLOYEES"))
								.irsTaxExemption(rset.getString("IRS_TAX_EXEMPTION"))
								.federalEmployerId(rset.getString("FEDERAL_EMPLOYER_ID"))
								.massTaxExemptNum(rset.getString("MASS_TAX_EXEMPT_NUM"))
								.agencySymbol(rset.getString("AGENCY_SYMBOL"))
								.vendorCode(rset.getString("VENDOR_CODE"))
								.comGovEntityCode(rset.getString("COM_GOV_ENTITY_CODE"))
								.massEmployeeClaim(rset.getString("MASS_EMPLOYEE_CLAIM"))
								.humanSubAssurance(rset.getString("HUMAN_SUB_ASSURANCE"))
								.animalWelfareAssurance(rset.getString("ANIMAL_WELFARE_ASSURANCE"))
								.scienceMisconductComplDate(rset.getDate("SCIENCE_MISCONDUCT_COMPL_DATE"))
								.phsAcount(rset.getString("PHS_ACOUNT"))
								.nsfInstitutionalCode(rset.getString("NSF_INSTITUTIONAL_CODE"))
								.indirectCostRateAgreement(rset.getString("INDIRECT_COST_RATE_AGREEMENT"))
								.cognizantAuditor(rset.getInt("COGNIZANT_AUDITOR"))
								.onrResidentRep(rset.getInt("ONR_RESIDENT_REP"))
								.lobbyingRegistrant(rset.getString("LOBBYING_REGISTRANT"))
								.lobbyingIndividual(rset.getString("LOBBYING_INDIVIDUAL"))
								.samExpirationDate(rset.getDate("SAM_EXPIRATION_DATE"))
								.incorporationDate(rset.getDate("INCORPORATION_DATE"))
								.incorporatedIn(rset.getString("INCORPORATED_IN"))
								.riskLevel(rset.getString("RISK_LEVEL"))
								.build());
						}
						log.info("Stored procedure executed successfully, {} records found", results.size());
						return results;
					}
				}
			});
		} catch (DataAccessException e) {
			log.error("DataAccessException while executing stored procedure COI_INT_GET_ENTITY_DETAILS with entityId {}: {}", entityId, e.getMessage(), e);
		} catch (Exception e) {
			log.error("Unexpected error occurred while fetching entity details with entityId {}: {}", entityId, e.getMessage(), e);
		}
		return dtos;
	}

	public void updateEntitySponsorInfoByParams(EntityResponse entityResponse) {
		try {
			log.info("Calling stored procedure COI_INT_KC_SPON_ORG_RESPONSE with entityId: {}, sponsorCode: {}, sponsorFeedError: {}, orgFeedError: {}, organizationId: {}",
					entityResponse.getEntityId(), entityResponse.getSponsorCode(), entityResponse.getSponsorFeedError(), entityResponse.getOrganizationFeedError(), entityResponse.getOrganizationId());

			jdbcTemplate.execute((Connection conn) -> {
				try (CallableStatement cs = conn.prepareCall("{call COI_INT_KC_SPON_ORG_RESPONSE(?, ?, ?, ?, ?)}")) {
					cs.setInt(1, entityResponse.getEntityId());
					cs.setString(2, entityResponse.getOrganizationFeedError());
					cs.setString(3, entityResponse.getSponsorFeedError());
					cs.setString(4, entityResponse.getSponsorCode());
					cs.setString(5, entityResponse.getOrganizationId());
					cs.execute();
				}
				return null;
			});
		} catch (DataAccessException e) {
			log.error("DataAccessException while executing stored procedure COI_INT_KC_SPON_ORG_RESPONSE with entityId {}: {}", entityResponse.getEntityId(), e.getMessage(), e);
		} catch (Exception e) {
			log.error("Unexpected error occurred while updating entity sponsor info with entityId {}: {}", entityResponse.getEntityId(), e.getMessage(), e);
		}
	}

}
