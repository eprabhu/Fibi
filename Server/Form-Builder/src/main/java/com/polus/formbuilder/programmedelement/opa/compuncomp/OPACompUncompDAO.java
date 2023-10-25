package com.polus.formbuilder.programmedelement.opa.compuncomp;

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
import com.polus.formbuilder.programmedelement.opa.outsidefinancialinterest.OPAOutsideFinancialInterestResponseDTO;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.transaction.Transactional;

@Transactional
@Component
public class OPACompUncompDAO {

	@PersistenceContext
	private EntityManager entityManager;
	
	@Autowired
	private OPADiscActivityEntityRepository repository;
	
	@Autowired
	private OPACommonDAO opaCommonDAO;
	
	@Autowired
	private HibernateTemplate hibernateTemplate;

	public List<OPACompUncompResponseDTO> getPEComponentDetails(
			OPACompUncompRequestModel opaRequest) {
		List<OPADiscActivityEntity> lsDiscActivity = repository
				.fetchAllByOPADisclosureID(opaRequest.getOpaDisclosureId());
		List<OPACompUncompResponseDTO> output = prepareResponseDTO(opaRequest.getOpaDisclosureId(), lsDiscActivity);
		return output;
	}
	

	@Transactional
	public List<OPACompUncompResponseDTO> savePEComponent(OPACompUncompRequestModel opaRequest) {
		if (opaRequest.getOpaDisclActivityId() != null) {
			return updatePEComponent(opaRequest);
		}
		return addPEComponent(opaRequest);
	}

	
	private List<OPACompUncompResponseDTO> addPEComponent(OPACompUncompRequestModel opaRequest) {
		
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
		List<OPADiscActivityEntity> lsDiscActivity = InsertPEComponent(opaRequest);

		List<OPACompUncompResponseDTO> output = prepareResponseDTO(opaRequest.getOpaDisclosureId(), lsDiscActivity);
		return output;

	}

	private List<OPADiscActivityEntity> InsertPEComponent(OPACompUncompRequestModel opaRequest) {
		List<OPADiscActivityEntity> lsDiscActivity = new ArrayList<>();
		OPADiscActivityEntity activityEntity = mapRequestToEntity(opaRequest);
		hibernateTemplate.saveOrUpdate(activityEntity);
		lsDiscActivity.add(activityEntity);
		return lsDiscActivity;
	}
	
	private List<OPACompUncompResponseDTO> updatePEComponent(OPACompUncompRequestModel opaRequest) {
		
		OPADiscActivityEntity activityEntity = mapRequestToEntity(opaRequest);
		try {
			hibernateTemplate.saveOrUpdate(activityEntity);
		} catch (Exception e) {
			throw new RuntimeException("Error OPACompUncompDAO - in updatePEComponent() --> " + e.getMessage());
		}

		return prepareResponse(activityEntity, opaRequest);

	}

	private OPADiscActivityEntity mapRequestToEntity(OPACompUncompRequestModel opaRequest) {
		OPADiscActivityEntity activityEntity = OPADiscActivityEntity.builder()
				.opaDisclActivityId(opaRequest.getOpaDisclActivityId()).opaDisclosureId(opaRequest.getOpaDisclosureId())
				.opaDisclPersonEntityId(opaRequest.getOpaDisclPersonEntityId())
				.isCompensated(opaRequest.getIsCompensated()).numOfDaysAcademic(opaRequest.getNumOfDaysAcademic())
				.numOfDaysInYear(opaRequest.getNumOfDaysInYear()).numOfDaysSummer(opaRequest.getNumOfDaysSummer())
				.natureOfWork(opaRequest.getNatureOfWork()).description1(opaRequest.getDescription1())
				.description2(opaRequest.getDescription2()).updateTimestamp(new Date())
				.updateUser(opaRequest.getUpdateUser()).build();

		return activityEntity;
	}

	private List<OPACompUncompResponseDTO> prepareResponse(OPADiscActivityEntity activity,
			OPACompUncompRequestModel opaRequest) {
		List<OPACompUncompResponseDTO> output = new ArrayList<>();

		output.add(OPACompUncompResponseDTO.builder().opaDisclActivityId(activity.getOpaDisclActivityId())
				.opaDisclosureId(activity.getOpaDisclosureId())
				.opaDisclPersonEntityId(activity.getOpaDisclPersonEntityId())
				.personEntityId(opaRequest.getEntityInfo().getPersonEntityId()).natureOfWork(activity.getNatureOfWork())
				.description1(activity.getDescription1()).description2(activity.getDescription2())
				.isCompensated(activity.getIsCompensated()).numOfDaysAcademic(activity.getNumOfDaysAcademic())
				.numOfDaysInYear(activity.getNumOfDaysInYear()).numOfDaysSummer(activity.getNumOfDaysSummer())
				.updateTimestamp(activity.getUpdateTimestamp()).updateUser(activity.getUpdateUser())
				.entityInfo(opaRequest.getEntityInfo()).build());

		return output;
	}

	

	@Transactional
	public void deletePEComponent(OPACompUncompRequestModel opaRequest) {
		OPADiscActivityEntity disclosureActivity = mapRequestToEntity(opaRequest);
		if (disclosureActivity != null) {
			hibernateTemplate.delete(disclosureActivity);
		}
		// TODO - need to sync the OPA DISCL PERSON ENTITY and PERSON ENTITY
		// repository.deleteById(opaRequest.getOpaDisclActivityId());

	}

	private List<OPACompUncompResponseDTO> prepareResponseDTO(Integer OPADisclosureId,
			List<OPADiscActivityEntity> lsDiscActivity) {

		List<OPACompUncompResponseDTO> output = new ArrayList<>();
		Map<Integer, OPAPersonEntityInfoDTO> personInfoMap = opaCommonDAO.getOPAPersonEntityInfo(OPADisclosureId);

		for (OPADiscActivityEntity activity : lsDiscActivity) {
			OPAPersonEntityInfoDTO personInfo = personInfoMap.get(activity.getOpaDisclPersonEntityId());
			if (personInfo != null) {
				output.add(OPACompUncompResponseDTO.builder().opaDisclActivityId(activity.getOpaDisclActivityId())
						.opaDisclosureId(activity.getOpaDisclosureId())
						.opaDisclPersonEntityId(activity.getOpaDisclPersonEntityId())
						.personEntityId(personInfo.getPersonEntityId()).natureOfWork(activity.getNatureOfWork())
						.description1(activity.getDescription1()).description2(activity.getDescription2())
						.isCompensated(activity.getIsCompensated()).numOfDaysAcademic(activity.getNumOfDaysAcademic())
						.numOfDaysInYear(activity.getNumOfDaysInYear()).numOfDaysSummer(activity.getNumOfDaysSummer())
						.updateTimestamp(activity.getUpdateTimestamp()).updateUser(activity.getUpdateUser())
						.entityInfo(personInfo).build());
			}

		}
		
		return output;

	}
	
}
