package com.polus.kcintegration.entity.dao;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.kcintegration.entity.dto.EntityDTO;
import com.polus.kcintegration.entity.dto.EntityResponse;
import com.polus.kcintegration.exception.custom.IntegrationCustomException;

import jakarta.persistence.EntityManager;
import jakarta.persistence.ParameterMode;
import jakarta.persistence.StoredProcedureQuery;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Transactional
@Service
public class EntityKCIntegrationDao {

	@Autowired
	private EntityManager entityManager;

	public EntityResponse feedEntityDetailsToSponsorAndOrg(EntityDTO entityDTO) {
		EntityResponse entityResponse = null;
		ResultSet rset = null;
		StoredProcedureQuery storedProcedure = null;

		try {
			String procedureName = "FIBI_COI_FEED_ENTITY";
			storedProcedure = entityManager.createStoredProcedureQuery(procedureName);

			registerStoredProcedureParameters(storedProcedure);

			setStoredProcedureParameters(storedProcedure, entityDTO);

			boolean executeResult = storedProcedure.execute();

			if (executeResult) {
				ResultSet resultSet = (ResultSet) storedProcedure.getOutputParameterValue(54);
				if (resultSet != null) {
					while (resultSet.next()) {
						entityResponse = EntityResponse.builder().entityId(entityDTO.getEntityId())
								.organizationFeedError(resultSet.getString("ORG_ERROR"))
								.organizationId(resultSet.getString("ORGANIZATION_ID"))
								.sponsorCode(resultSet.getString("SPONSOR_CODE"))
								.sponsorFeedError(resultSet.getString("SPONSOR_ERROR")).build();
					}
				}
			}

			log.info("Stored procedure {} executed successfully for entityId: {}", procedureName, entityDTO.getEntityId());

		} catch (SQLException e) {
			log.error("SQLException in feedEntityDetailsToSponsorAndOrg for entityId {}: {}", entityDTO.getEntityId(), e.getMessage(), e);
			throw new IntegrationCustomException("Database error during feedEntityDetailsToSponsorAndOrg", e, entityDTO.getEntityId());
		} catch (Exception e) {
			e.printStackTrace();
			log.error("Unexpected exception in feedEntityDetailsToSponsorAndOrg for entityId {}: {}", entityDTO.getEntityId(), e.getMessage(), e);
			throw new IntegrationCustomException("Unexpected error during feedEntityDetailsToSponsorAndOrg", e, entityDTO.getEntityId());
		} finally {
			closeResources(rset, storedProcedure, entityDTO.getEntityId());
		}

		return entityResponse;
	}

	private void registerStoredProcedureParameters(StoredProcedureQuery storedProcedure) {
		for (int i = 1; i <= 54; i++) {
			if (i == 54) {
				storedProcedure.registerStoredProcedureParameter(i, void.class, ParameterMode.REF_CURSOR);
			} else if (i == 3 || i == 4 || i == 32 || i == 46 || i == 47) {
				storedProcedure.registerStoredProcedureParameter(i, Integer.class, ParameterMode.IN);
			} else if (i == 42 || i == 50 || i == 51) {
				storedProcedure.registerStoredProcedureParameter(i, Date.class, ParameterMode.IN);
			} else if (i == 5 || i == 6) {
				storedProcedure.registerStoredProcedureParameter(i, Boolean.class, ParameterMode.IN);
			} else {
				storedProcedure.registerStoredProcedureParameter(i, String.class, ParameterMode.IN);
			}
		}
	}

	private void setStoredProcedureParameters(StoredProcedureQuery storedProcedure, EntityDTO entityDTO) {
		storedProcedure.setParameter(1, entityDTO.getSponsorCode());
		storedProcedure.setParameter(2, entityDTO.getOrganizationId());
		storedProcedure.setParameter(3, entityDTO.getEntityId());
		storedProcedure.setParameter(4, entityDTO.getRolodexId());
		storedProcedure.setParameter(5, entityDTO.getIsCreateSponsor());
		storedProcedure.setParameter(6, entityDTO.getIsCreateOrganization());
		storedProcedure.setParameter(7, entityDTO.getPrimaryName());
		storedProcedure.setParameter(8, entityDTO.getSponsorAddressLine1());
		storedProcedure.setParameter(9, entityDTO.getSponsorAddressLine2());
		storedProcedure.setParameter(10, entityDTO.getSponsorCity());
		storedProcedure.setParameter(11, entityDTO.getSponsorState());
		storedProcedure.setParameter(12, entityDTO.getSponsorPostCode());
		storedProcedure.setParameter(13, entityDTO.getSponsorCountryCode());
		storedProcedure.setParameter(14, entityDTO.getOrgAddressLine1());
		storedProcedure.setParameter(15, entityDTO.getOrgAddressLine2());
		storedProcedure.setParameter(16, entityDTO.getOrgCity());
		storedProcedure.setParameter(17, entityDTO.getOrgState());
		storedProcedure.setParameter(18, entityDTO.getOrgPostCode());
		storedProcedure.setParameter(19, entityDTO.getOrgCountryCode());
		storedProcedure.setParameter(20, entityDTO.getCertifiedEmail());
		storedProcedure.setParameter(21, entityDTO.getTelephoneNumber());
		storedProcedure.setParameter(22, entityDTO.getUpdatedBy());
		storedProcedure.setParameter(23, entityDTO.getSponsorTypeCode());
		storedProcedure.setParameter(24, entityDTO.getCustomerNumber());
		storedProcedure.setParameter(25, entityDTO.getAuditReportSentForFy());
		storedProcedure.setParameter(26, entityDTO.getUeiNumber());
		storedProcedure.setParameter(27, entityDTO.getDodacNumber());
		storedProcedure.setParameter(28, entityDTO.getDunsNumber());
		storedProcedure.setParameter(29, entityDTO.getAcronym());
		storedProcedure.setParameter(30, entityDTO.getCageNumber());
		storedProcedure.setParameter(31, entityDTO.getDunningCampaignId());
		storedProcedure.setParameter(32, entityDTO.getNumberOfEmployees());
		storedProcedure.setParameter(33, entityDTO.getIrsTaxExemption());
		storedProcedure.setParameter(34, entityDTO.getFederalEmployerId());
		storedProcedure.setParameter(35, entityDTO.getMassTaxExemptNum());
		storedProcedure.setParameter(36, entityDTO.getAgencySymbol());
		storedProcedure.setParameter(37, entityDTO.getVendorCode());
		storedProcedure.setParameter(38, entityDTO.getComGovEntityCode());
		storedProcedure.setParameter(39, entityDTO.getMassEmployeeClaim());
		storedProcedure.setParameter(40, entityDTO.getHumanSubAssurance());
		storedProcedure.setParameter(41, entityDTO.getAnimalWelfareAssurance());
		storedProcedure.setParameter(42, entityDTO.getScienceMisconductComplDate());
		storedProcedure.setParameter(43, entityDTO.getPhsAcount());
		storedProcedure.setParameter(44, entityDTO.getNsfInstitutionalCode());
		storedProcedure.setParameter(45, entityDTO.getIndirectCostRateAgreement());
		storedProcedure.setParameter(46, entityDTO.getCognizantAuditor());
		storedProcedure.setParameter(47, entityDTO.getOnrResidentRep());
		storedProcedure.setParameter(48, entityDTO.getLobbyingRegistrant());
		storedProcedure.setParameter(49, entityDTO.getLobbyingIndividual());
		storedProcedure.setParameter(50, entityDTO.getSamExpirationDate());
		storedProcedure.setParameter(51, entityDTO.getIncorporationDate());
		storedProcedure.setParameter(52, entityDTO.getIncorporatedIn());
		storedProcedure.setParameter(53, entityDTO.getRiskLevel());
	}

	private void closeResources(ResultSet resultSet, StoredProcedureQuery storedProcedure, Integer entityId) {
		if (resultSet != null) {
			try {
				resultSet.close();
			} catch (SQLException e) {
				log.warn("Error closing ResultSet for entityId {}: {}", entityId, e.getMessage(), e);
			}
		}

		if (entityManager != null) {
			entityManager.close();
		}
	}

}
