package com.polus.formbuilder.programmedelement.opa.studentsubordinateinvolvement;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.hibernate.query.NativeQuery;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Component;

import com.polus.formbuilder.programmedelement.opa.common.OPACommonDAO;
import com.polus.formbuilder.programmedelement.opa.common.OPADisclPersonEntity;
import com.polus.formbuilder.programmedelement.opa.common.OPAPersonEntityInfoDTO;
import com.polus.formbuilder.programmedelement.opa.common.OPAPersonTypeRepository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.transaction.Transactional;

@Transactional
@Component
public class OPAStudentSubordinateInvolvementDAO {

	@PersistenceContext
	private EntityManager entityManager;

	@Autowired
	private OPAStudentSubordinateInvolvementRepository repository;

	@Autowired
	private OPAPersonTypeRepository opaPersonTypeRepository;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private OPACommonDAO opaCommonDAO;

	public List<OPAStudentSubordinateInvolvementResponseDTO> getPEComponentDetails(OPAStudentSubordinateInvolvementRequestModel opaRequest) {
		List<OPAStudentSubordinateInvolvementEntity> lsStudentSubordinateInvolvement = repository.fetchAllByOPADisclosureID(opaRequest.getOpaDisclosureId());
		return prepareResponseDTO(opaRequest.getOpaDisclosureId(), lsStudentSubordinateInvolvement);
	}

	private List<OPAStudentSubordinateInvolvementResponseDTO> prepareResponseDTO(Integer opaDisclosureId, List<OPAStudentSubordinateInvolvementEntity> lsStudentSubordinateInvolvement) {
		List<OPAStudentSubordinateInvolvementResponseDTO> output = new ArrayList<>();

		Map<Integer, OPAPersonEntityInfoDTO> personInfoMap = opaCommonDAO.getOPAPersonEntityInfo(opaDisclosureId);
		
		for (OPAStudentSubordinateInvolvementEntity entity : lsStudentSubordinateInvolvement) {
			OPAPersonEntityInfoDTO personInfo = personInfoMap.get(entity.getOpaDisclPersonEntityId());
			if (personInfo != null) {
				output.add(OPAStudentSubordinateInvolvementResponseDTO.builder()
						.opaStudSubInvId(entity.getOpaStudSubInvId())
						.opaDisclosureId(entity.getOpaDisclosureId())
						.opaDisclPersonEntityId(entity.getOpaDisclPersonEntityId())
						.opaPersonTypeCode(entity.getOpaPersonTypeCode())
						.opaPersonType(opaPersonTypeRepository.fetchTypeByPersonTypeCode(entity.getOpaPersonTypeCode()))
						.personEntityId(personInfo.getPersonEntityId())
						.description1(entity.getDescription1())
						.description2(entity.getDescription2())
						.personId(entity.getPersonId())
		                .personName(getPersonFullNameByPersonId(entity.getPersonId()))
		                .natureOfWork(entity.getNatureOfWork())
		                .relationWithPerson(entity.getRelationWithPerson())
						.updateTimestamp(entity.getUpdateTimestamp())
						.updateUser(entity.getUpdateUser())
						.entityInfo(personInfo).build());
			}

		}
		return output;
	}

	private String getPersonFullNameByPersonId(String personId) {
		NativeQuery<?> query = (NativeQuery<?>) entityManager.createNativeQuery("SELECT FULL_NAME FROM PERSON where PERSON_ID = :personId");
	    query.setParameter("personId", personId);
	    return (String) query.uniqueResult();
	}

	public List<OPAStudentSubordinateInvolvementResponseDTO> savePEComponent(
			OPAStudentSubordinateInvolvementRequestModel opaRequest) {
		if (opaRequest.getOpaStudSubInvId() != null) {
			return updatePEComponent(opaRequest);
		}
		return addPEComponent(opaRequest);
	}

	private List<OPAStudentSubordinateInvolvementResponseDTO> addPEComponent(OPAStudentSubordinateInvolvementRequestModel opaRequest) {
		
		OPADisclPersonEntity opaPersonEntity = opaCommonDAO.getOPAPersonEntity( opaRequest.getOpaDisclosureId(), 
				opaRequest.getPersonEntityId(),
				opaRequest.getUpdateUser());	
		
		opaRequest.setOpaDisclPersonEntityId(opaPersonEntity.getOpaDisclPersonEntityId());
		List<OPAStudentSubordinateInvolvementEntity> lsEntity = InsertPEComponent(opaRequest);
		List<OPAStudentSubordinateInvolvementResponseDTO> output = prepareAddPEResponseDTO(opaRequest.getPersonEntityId(), lsEntity);
		
		return output;
	}

	private List<OPAStudentSubordinateInvolvementEntity> InsertPEComponent(OPAStudentSubordinateInvolvementRequestModel opaRequest) {
		List<OPAStudentSubordinateInvolvementEntity> lsEntity = new ArrayList<>();
		OPAStudentSubordinateInvolvementEntity entity = mapRequestToEntity(opaRequest);
		hibernateTemplate.saveOrUpdate(entity);
		lsEntity.add(entity);
		return lsEntity;
	}
	
	private List<OPAStudentSubordinateInvolvementResponseDTO> prepareAddPEResponseDTO(Integer personEntityId, List<OPAStudentSubordinateInvolvementEntity> lsEntity) {

		List<OPAStudentSubordinateInvolvementResponseDTO> output = new ArrayList<>();
		
		Map<Integer, OPAPersonEntityInfoDTO> personInfoMap = opaCommonDAO.getPersonEntityInfo(personEntityId);

		for (OPAStudentSubordinateInvolvementEntity entity : lsEntity) {
			OPAPersonEntityInfoDTO personInfo = personInfoMap.get(personEntityId);
			if (personInfo != null) {
				output.add(OPAStudentSubordinateInvolvementResponseDTO.builder()
						.opaStudSubInvId(entity.getOpaStudSubInvId())
						.opaDisclosureId(entity.getOpaDisclosureId())
						.opaDisclPersonEntityId(entity.getOpaDisclPersonEntityId())
						.opaPersonTypeCode(entity.getOpaPersonTypeCode())
						.opaPersonType(opaPersonTypeRepository.fetchTypeByPersonTypeCode(entity.getOpaPersonTypeCode()))
						.personEntityId(personInfo.getPersonEntityId())
						.description1(entity.getDescription1())
						.description2(entity.getDescription2())
						.personId(entity.getPersonId())
		                .personName(getPersonFullNameByPersonId(entity.getPersonId()))
		                .natureOfWork(entity.getNatureOfWork())
		                .relationWithPerson(entity.getRelationWithPerson())
						.updateTimestamp(entity.getUpdateTimestamp())
						.updateUser(entity.getUpdateUser())
						.entityInfo(personInfo).build());
			}

		}

		return output;

	}

	private List<OPAStudentSubordinateInvolvementResponseDTO> updatePEComponent(OPAStudentSubordinateInvolvementRequestModel opaRequest) {
		OPAStudentSubordinateInvolvementEntity entity = mapRequestToEntity(opaRequest);
		try {
			hibernateTemplate.saveOrUpdate(entity);
		} catch (Exception e) {
			throw new RuntimeException("Error OPAStudentSubordinateInvolvementComponent - in updatePEComponent() --> " + e.getMessage());
		}
		return prepareResponse(entity, opaRequest);
	}

	private List<OPAStudentSubordinateInvolvementResponseDTO> prepareResponse(
			OPAStudentSubordinateInvolvementEntity entity, OPAStudentSubordinateInvolvementRequestModel opaRequest) {

		List<OPAStudentSubordinateInvolvementResponseDTO> output = new ArrayList<>();

		output.add(OPAStudentSubordinateInvolvementResponseDTO.builder()
				.opaStudSubInvId(entity.getOpaStudSubInvId())
				.opaDisclosureId(entity.getOpaDisclosureId())
				.opaDisclPersonEntityId(entity.getOpaDisclPersonEntityId())
				.opaPersonTypeCode(entity.getOpaPersonTypeCode())
				.opaPersonType(opaPersonTypeRepository.fetchTypeByPersonTypeCode(entity.getOpaPersonTypeCode()))
				.personEntityId(opaRequest.getEntityInfo().getPersonEntityId())
				.description1(entity.getDescription1())
				.description2(entity.getDescription2())
				.personId(entity.getPersonId())
                .personName(getPersonFullNameByPersonId(entity.getPersonId()))
                .natureOfWork(entity.getNatureOfWork())
                .relationWithPerson(entity.getRelationWithPerson())
				.updateTimestamp(entity.getUpdateTimestamp())
				.updateUser(entity.getUpdateUser())
				.entityInfo(opaRequest.getEntityInfo()).build());

		return output;
	}

	private OPAStudentSubordinateInvolvementEntity mapRequestToEntity(OPAStudentSubordinateInvolvementRequestModel opaRequest) {
		return OPAStudentSubordinateInvolvementEntity.builder()
				.opaStudSubInvId(opaRequest.getOpaStudSubInvId())
				.opaDisclosureId(opaRequest.getOpaDisclosureId())
				.opaPersonTypeCode(opaRequest.getOpaPersonTypeCode())
				.personId(opaRequest.getPersonId())
				.natureOfWork(opaRequest.getNatureOfWork())
				.relationWithPerson(opaRequest.getRelationWithPerson())
				.description1(opaRequest.getDescription1())
				.description2(opaRequest.getDescription2())
				.opaDisclPersonEntityId(opaRequest.getOpaDisclPersonEntityId())
				.numOfDays(opaRequest.getNumOfDays())
				.updateTimestamp(new Date())
				.updateUser(opaRequest.getUpdateUser())
				.build();
	}

	@Transactional
	public void deletePEComponent(OPAStudentSubordinateInvolvementRequestModel opaRequest) {
		OPAStudentSubordinateInvolvementEntity entity = mapRequestToEntity(opaRequest);
		if (entity != null) {
			hibernateTemplate.delete(entity);
		}
	}

}
