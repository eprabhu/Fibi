package com.polus.formbuilder.programmedelement.opa.outsidefinancialinterest;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Component;

import com.polus.formbuilder.programmedelement.opa.common.OPACommonDAO;
import com.polus.formbuilder.programmedelement.opa.common.OPADisclPersonEntity;
import com.polus.formbuilder.programmedelement.opa.common.OPAPersonEntityInfoDTO;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.transaction.Transactional;

@Transactional
@Component
public class OPAOutsideFinancialInterestDAO {

	@PersistenceContext
	private EntityManager entityManager;
	
	@Autowired
	private OPAOutsideFinancialInterestRepository repository;
	
	@Autowired
	private OPACommonDAO opaCommonDAO;
	
	@Autowired
	private HibernateTemplate hibernateTemplate;

	public List<OPAOutsideFinancialInterestResponseDTO> getPEComponentDetails(
			OPAOutsideFinancialInterestRequestModel opaRequest) {
		List<OPAOutsideFinancialInterestEntity> lsOutsideFinancial = repository
																.fetchAllByOPADisclosureID(opaRequest.getOpaDisclosureId());
		List<OPAOutsideFinancialInterestResponseDTO> output = prepareResponseDTO(opaRequest.getOpaDisclosureId(), lsOutsideFinancial);
		return output;
	}
	

	@Transactional
	public List<OPAOutsideFinancialInterestResponseDTO> savePEComponent(OPAOutsideFinancialInterestRequestModel opaRequest) {
		if (opaRequest.getOpaOutsideFinancialInterestId() != null) {
			return updatePEComponent(opaRequest);
		}
		return addPEComponent(opaRequest);
	}

	
	private List<OPAOutsideFinancialInterestResponseDTO> addPEComponent(OPAOutsideFinancialInterestRequestModel opaRequest) {
		
		OPADisclPersonEntity opaPersonEntity = opaCommonDAO.getOPAPersonEntity( opaRequest.getOpaDisclosureId(), 
																				opaRequest.getPersonEntityId(),
																				opaRequest.getUpdateUser());	
		
		// START - If there is no entry in OPA_DISCL_PERSON_ENTITY table against an
		// PERSON_ENTITY then Sync OPA_DISCL_PERSON_ENTITY table
		if (opaPersonEntity == null) {
			opaPersonEntity = opaCommonDAO.SyncOPADisclPersonEntity(opaRequest.getOpaDisclosureId(), 
																	opaRequest.getPersonEntityId(),
																	opaRequest.getUpdateUser());
		}
		// END - If there is no entry in OPA_DISCL_PERSON_ENTITY table against an
		// PERSON_ENTITY then Sync OPA_DISCL_PERSON_ENTITY table
		
		opaRequest.setOpaDisclPersonEntityId(opaPersonEntity.getOpaDisclPersonEntityId());
		List<OPAOutsideFinancialInterestEntity> lsFinInterest = InsertPEComponent(opaRequest);

		List<OPAOutsideFinancialInterestResponseDTO> output = prepareResponseDTO(opaRequest.getOpaDisclosureId(), lsFinInterest);
	
		return output;
	}

	private List<OPAOutsideFinancialInterestEntity> InsertPEComponent(OPAOutsideFinancialInterestRequestModel opaRequest) {
		List<OPAOutsideFinancialInterestEntity> lsFinInterest = new ArrayList<>();
		OPAOutsideFinancialInterestEntity finInterestEntity = mapRequestToEntity(opaRequest);
		hibernateTemplate.saveOrUpdate(finInterestEntity);
		lsFinInterest.add(finInterestEntity);
		return lsFinInterest;
	}
	
	private List<OPAOutsideFinancialInterestResponseDTO> updatePEComponent(OPAOutsideFinancialInterestRequestModel opaRequest) {
		
		OPAOutsideFinancialInterestEntity finInterestEntity = mapRequestToEntity(opaRequest);
		try {
			hibernateTemplate.saveOrUpdate(finInterestEntity);
		} catch (Exception e) {
			throw new RuntimeException("Error OPAOutsideFinancialComponent - in updatePEComponent() --> " + e.getMessage());
		}

		return prepareResponse(finInterestEntity, opaRequest);

	}

	private OPAOutsideFinancialInterestEntity mapRequestToEntity(OPAOutsideFinancialInterestRequestModel opaRequest) {
		OPAOutsideFinancialInterestEntity activityEntity = OPAOutsideFinancialInterestEntity.builder()
				.opaOutsideFinancialInterestId(opaRequest.getOpaOutsideFinancialInterestId())
				.opaDisclosureId(opaRequest.getOpaDisclosureId())
				.opaDisclPersonEntityId(opaRequest.getOpaDisclPersonEntityId())
				.personsRelationWithEntity(opaRequest.getPersonsRelationWithEntity())
				.entityRelationWithInstitute(opaRequest.getEntityRelationWithInstitute())
				.description1(opaRequest.getDescription1())
				.description2(opaRequest.getDescription2())
				.updateTimestamp(new Date())
				.updateUser(opaRequest.getUpdateUser()).build();

		return activityEntity;
	}

	private List<OPAOutsideFinancialInterestResponseDTO> prepareResponse(OPAOutsideFinancialInterestEntity finInterest,
			OPAOutsideFinancialInterestRequestModel opaRequest) {
		List<OPAOutsideFinancialInterestResponseDTO> output = new ArrayList<>();

		output.add(OPAOutsideFinancialInterestResponseDTO.builder()
				.opaOutsideFinancialInterestId(finInterest.getOpaOutsideFinancialInterestId())
				.opaDisclosureId(finInterest.getOpaDisclosureId())
				.opaDisclPersonEntityId(finInterest.getOpaDisclPersonEntityId())
				.personEntityId(opaRequest.getEntityInfo().getPersonEntityId())
				.personsRelationWithEntity(finInterest.getPersonsRelationWithEntity())
				.entityRelationWithInstitute(finInterest.getEntityRelationWithInstitute())
				.description1(finInterest.getDescription1())
				.description2(finInterest.getDescription2())				
				.updateTimestamp(finInterest.getUpdateTimestamp())
				.updateUser(finInterest.getUpdateUser())
				.entityInfo(opaRequest.getEntityInfo()).build());

		return output;
	}

	

	@Transactional
	public void deletePEComponent(OPAOutsideFinancialInterestRequestModel opaRequest) {
		OPAOutsideFinancialInterestEntity disclosureActivity = mapRequestToEntity(opaRequest);
		if (disclosureActivity != null) {
			hibernateTemplate.delete(disclosureActivity);
		}
		// TODO - need to sync the OPA DISCL PERSON ENTITY and PERSON ENTITY
		// repository.deleteById(opaRequest.getOpaDisclActivityId());

	}

	private List<OPAOutsideFinancialInterestResponseDTO> prepareResponseDTO(Integer OPADisclosureId,
			List<OPAOutsideFinancialInterestEntity> lsFinacialInterest) {

		List<OPAOutsideFinancialInterestResponseDTO> output = new ArrayList<>();
		Map<Integer, OPAPersonEntityInfoDTO> personInfoMap = opaCommonDAO.getOPAPersonEntityInfo(OPADisclosureId);

		for (OPAOutsideFinancialInterestEntity finInterest : lsFinacialInterest) {
			OPAPersonEntityInfoDTO personInfo = personInfoMap.get(finInterest.getOpaDisclPersonEntityId());
			if (personInfo != null) {
				output.add(OPAOutsideFinancialInterestResponseDTO.builder()
						.opaOutsideFinancialInterestId(finInterest.getOpaOutsideFinancialInterestId())
						.opaDisclosureId(finInterest.getOpaDisclosureId())
						.opaDisclPersonEntityId(finInterest.getOpaDisclPersonEntityId())
						.personEntityId(personInfo.getPersonEntityId())
						.personsRelationWithEntity(finInterest.getPersonsRelationWithEntity())
						.entityRelationWithInstitute(finInterest.getEntityRelationWithInstitute())
						.description1(finInterest.getDescription1())
						.description2(finInterest.getDescription2())						
						.updateTimestamp(finInterest.getUpdateTimestamp())
						.updateUser(finInterest.getUpdateUser())
						.entityInfo(personInfo).build());
			}

		}
		
		return output;

	}
	
}
