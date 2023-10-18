package com.polus.formbuilder.programmedelement.opa.compuncomp;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.polus.formbuilder.dto.FormBuilderSectionsComponentDTO;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;
import com.polus.formbuilder.programmedelement.opa.entity.OPADiscActivityEntity;
import com.polus.formbuilder.programmedelement.opa.repository.OPADisclPersonEntityRepository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.ParameterMode;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;
import jakarta.persistence.StoredProcedureQuery;
import jakarta.persistence.TypedQuery;
import jakarta.transaction.Transactional;

@Transactional
@Component
public class OPACompUnCompDAO {

	@PersistenceContext
	private EntityManager entityManager;
	
	@Autowired
	private OPADiscActivityEntityRepository repository;
	
	@Autowired
	private OPADisclPersonEntityRepository disclPersonEntityRepository;
	
	
	public List<OPACompUnCompResponseDTO> getDisclosureActivity(OPACompUnCompRequestModel opaRequest) {
		
		List<OPAPersonEntityInfoDTO>  lsPersonEntityInfo = getPersonEntityInfo(opaRequest.getOpaDisclosureId());
		List<OPADiscActivityEntity> lsDiscActivity = repository.fetchAllByOPADisclosureID(opaRequest.getOpaDisclosureId());
		List<OPACompUnCompResponseDTO> output = new ArrayList<>();
	
		for(OPADiscActivityEntity activity : lsDiscActivity) {
			OPAPersonEntityInfoDTO personInfoDto = new OPAPersonEntityInfoDTO();
				for(OPAPersonEntityInfoDTO personInfo : lsPersonEntityInfo) {
					
					if(personInfo.getOpaDisclPersonEntityId().equals(activity.getOpaDisclPersonEntityId())) {
						
//						personInfoDto = OPAPersonEntityInfoDTO.builder()
//														.opaPersonEntityId(personInfo.getOpaPersonEntityId())
//														.personEntityId(personInfo.getPersonEntityId())
//														.personId(personInfo.getPersonId())
//														.entityNumber(personInfo.getEntityNumber())
//														.entityName(personInfo.getEntityName())
//														.entityType(personInfo.getEntityType())
//														.countryName(personInfo.getCountryName())
//														.relationship(personInfo.getRelationship())
//														.involvementStartDate(personInfo.getInvolvementStartDate())
//														.involvementEndDate(personInfo.getInvolvementEndDate())
//														.build();
						
						personInfoDto = personInfo;
						
						break;
					}
					
				}
			
				output.add(
							OPACompUnCompResponseDTO.builder()
										.opaDiscActivityId(activity.getOpaDisclActivityId())
										.opaDisclosureId(activity.getOpaDisclosureId())
										.opaDisclPersonEntityId(activity.getOpaDisclPersonEntityId())
										.personEntityId(personInfoDto.getPersonEntityId())
										.natureOfWork(activity.getNatureOfWork())
										.description1(activity.getDescription1())
										.description2(activity.getDescription2())
										.isCompensated(activity.getIsCompensated())
										.numOfDaysAcademic(activity.getNumOfDaysAcademic())
										.numOfDaysInYear(activity.getNumOfDaysInYear())
										.numOfDaysSummer(activity.getNumOfDaysSummer())
										.updateTimestamp(activity.getUpdateTimestamp())
										.updateUser(activity.getUpdateUser())
										.entityInfo(personInfoDto)
										.build()
								);
		}
		
		return output;
	}
	

	@Transactional
	public void personSyncOPAPersonEntityAction(Integer opaDisclosureId, String updateUser) {
		StoredProcedureQuery query = entityManager.createStoredProcedureQuery("PROC_SYNC_OPA_PER_ENTITY")
				.registerStoredProcedureParameter(1, Integer.class, ParameterMode.IN)
				.registerStoredProcedureParameter(2, String.class, ParameterMode.IN);

		query.setParameter(1, opaDisclosureId);
		query.setParameter(2, updateUser);
		query.execute();

	}

	@SuppressWarnings("unchecked")
	@Transactional
	public List<OPAPersonEntityInfoDTO> getPersonEntityInfo(Integer opaDisclosureId) {
		try {
		StoredProcedureQuery query = entityManager.createStoredProcedureQuery("GET_OPA_PERSON_ENTITY_DETAILS")
				.registerStoredProcedureParameter(1, Integer.class, ParameterMode.IN);

		query.setParameter(1, opaDisclosureId);
		query.execute();

		return query.getResultList().stream().map(this::convertObjectToDTO).toList();
		
		}catch(Exception e){
			System.out.println(e.getMessage());
			return (List<OPAPersonEntityInfoDTO>) new OPAPersonEntityInfoDTO();
		}
		
	}
	
	private OPAPersonEntityInfoDTO convertObjectToDTO(Object object) {
		OPAPersonEntityInfoDTO opaPersonEntityInfoDTO = new OPAPersonEntityInfoDTO();
	    if (object instanceof Object[] data) {
	    	opaPersonEntityInfoDTO.setOpaDisclPersonEntityId((Integer) data[0]);
	    	opaPersonEntityInfoDTO.setPersonEntityId((Integer) data[1]); 
	    	opaPersonEntityInfoDTO.setPersonId((String) data[2]);
	    	opaPersonEntityInfoDTO.setEntityNumber((Integer) data[3]);
	    	opaPersonEntityInfoDTO.setEntityName((String) data[4]);
	    	opaPersonEntityInfoDTO.setEntityType((String) data[5]);
	    	opaPersonEntityInfoDTO.setCountryName((String) data[6]);
	    	opaPersonEntityInfoDTO.setRelationship((String) data[7]);
	    	opaPersonEntityInfoDTO.setInvolvementStartDate((Date) data[8]);
	    	opaPersonEntityInfoDTO.setInvolvementEndDate((Date) data[9]);
	    	opaPersonEntityInfoDTO.setEntityStatus((String) data[10]);
	        
	        return opaPersonEntityInfoDTO;
	        
	    }else {
	        return opaPersonEntityInfoDTO;
	    }
	}


	public List<OPACompUnCompResponseDTO> saveDisclosureActivity(OPACompUnCompRequestModel opaRequest) {
		if(opaRequest.getOpaDisclActivityId() != null) {
			return updateOpaDisclActivity(opaRequest);
		}
		return addOpaDisclActivity(opaRequest);
	}


	private List<OPACompUnCompResponseDTO> addOpaDisclActivity(OPACompUnCompRequestModel opaRequest) {
		
		OPADisclPersonEntity opaPersonEntity = disclPersonEntityRepository.FetchByPersonEntityId(opaRequest.getPersonEntityId());
		
		//Sync OPA_DISCL_PERSON_ENTITY table by adding the new SFI
		if(opaPersonEntity == null) {
			opaPersonEntity = prepareOpaPersonEntity(opaRequest);
			opaPersonEntity = disclPersonEntityRepository.save(opaPersonEntity);
		}	
		
		opaRequest.setOpaDisclPersonEntityId(opaPersonEntity.getOpaDisclPersonEntityId());
		OPADiscActivityEntity activityEntity = mapRequestToEntity(opaRequest);
		repository.save(activityEntity);
		return getDisclosureActivity(opaRequest);
		
	}


	private List<OPACompUnCompResponseDTO> updateOpaDisclActivity(OPACompUnCompRequestModel opaRequest) {
		
		OPADiscActivityEntity activityEntity = mapRequestToEntity(opaRequest);
		repository.save(activityEntity);		
		return prepareResponse(activityEntity,opaRequest);
	}


	private OPADiscActivityEntity mapRequestToEntity(OPACompUnCompRequestModel opaRequest) {
		OPADiscActivityEntity  activityEntity = OPADiscActivityEntity.builder()
												.opaDisclosureId(opaRequest.getOpaDisclosureId())
												.opaDisclPersonEntityId(opaRequest.getOpaDisclPersonEntityId())
												.isCompensated(opaRequest.getIsCompensated())
												.numOfDaysAcademic(opaRequest.getNumOfDaysAcademic())
												.numOfDaysInYear(opaRequest.getNumOfDaysInYear())
												.numOfDaysSummer(opaRequest.getNumOfDaysSummer())
												.natureOfWork(opaRequest.getNatureOfWork())
												.description1(opaRequest.getDescription1())
												.description2(opaRequest.getDescription2())
												.updateTimestamp(new Date())
												.updateUser(opaRequest.getUpdateUser())
												.build();
		return activityEntity;
	}


	private List<OPACompUnCompResponseDTO> prepareResponse(OPADiscActivityEntity activity,OPACompUnCompRequestModel opaRequest) {
		List<OPACompUnCompResponseDTO> output = new ArrayList<>();
				
		output.add(
				OPACompUnCompResponseDTO.builder()
							.opaDiscActivityId(activity.getOpaDisclActivityId())
							.opaDisclosureId(activity.getOpaDisclosureId())							
							.opaDisclPersonEntityId(activity.getOpaDisclPersonEntityId())
							.personEntityId(opaRequest.getEntityInfo().getPersonEntityId())
							.natureOfWork(activity.getNatureOfWork())
							.description1(activity.getDescription1())
							.description2(activity.getDescription2())
							.isCompensated(activity.getIsCompensated())
							.numOfDaysAcademic(activity.getNumOfDaysAcademic())
							.numOfDaysInYear(activity.getNumOfDaysInYear())
							.numOfDaysSummer(activity.getNumOfDaysSummer())
							.updateTimestamp(activity.getUpdateTimestamp())
							.updateUser(activity.getUpdateUser())
							.entityInfo(opaRequest.getEntityInfo())
							.build()
					);
		
		return output;
	}
	
	private OPADisclPersonEntity prepareOpaPersonEntity(OPACompUnCompRequestModel opaRequest) {

		OPADisclPersonEntity personEntity = new OPADisclPersonEntity();
		personEntity.setOpaDisclosureId(opaRequest.getOpaDisclosureId());
		personEntity.setUpdateTimestamp(new Date());
		personEntity.setUpdateUser(opaRequest.getUpdateUser());
		
		String sql = "SELECT PERSON_ENTITY_ID,ENTITY_ID,ENTITY_NUMBER FROM PERSON_ENTITY WHERE PERSON_ENTITY_ID = :personEntityId ";
		Query query = entityManager.createNativeQuery(sql);
		query.setParameter(":personEntityId", opaRequest.getPersonEntityId());
		List<?> resultRows = query.getResultList();

		for (Object row : resultRows) {
			if (row instanceof Object[]) {
				Object[] rowData = (Object[]) row;
				personEntity.setPersonEntityId((Integer) rowData[0]);
				personEntity.setEntityId((Integer) rowData[1]);
				personEntity.setEntityNumber((Integer) rowData[2]);				
			}
		}
		

		return personEntity;
	}


	public void deleteDisclosureActivity(OPACompUnCompRequestModel opaRequest) {
		repository.deleteById(opaRequest.getOpaDisclActivityId());
		
	}
	
	
}
