package com.polus.formbuilder.programmedelement.opa.instituteresourceuse;

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
public class OPAInstituteResourceUseDAO {

	@PersistenceContext
	private EntityManager entityManager;

	@Autowired
	private OPAInstituteResourceUseRepository repository;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private OPACommonDAO opaCommonDAO;

	public List<OPAInstituteResourceUseResponseDTO> getPEComponentDetails(OPAInstituteResourceUseRequestModel opaRequest) {
		List<OPAInstituteResourceUseEntity> lsInstituteResourceUse = repository.fetchAllByOPADisclosureID(opaRequest.getOpaDisclosureId());
		return prepareResponseDTO(opaRequest.getOpaDisclosureId(), lsInstituteResourceUse);
	}

	private List<OPAInstituteResourceUseResponseDTO> prepareResponseDTO(Integer opaDisclosureId, List<OPAInstituteResourceUseEntity> lsInstituteResourceUse) {
		List<OPAInstituteResourceUseResponseDTO> output = new ArrayList<>();
		
		Map<Integer, OPAPersonEntityInfoDTO> personInfoMap = opaCommonDAO.getOPAPersonEntityInfo(opaDisclosureId);
		
		for (OPAInstituteResourceUseEntity entity : lsInstituteResourceUse) {
			OPAPersonEntityInfoDTO personInfo = personInfoMap.get(entity.getOpaDisclPersonEntityId());
			if (personInfo != null) {
				output.add(OPAInstituteResourceUseResponseDTO.builder()
						.opaInstResId(entity.getOpaInstResId())
						.opaDisclosureId(entity.getOpaDisclosureId())
						.opaDisclPersonEntityId(entity.getOpaDisclPersonEntityId())
						.personEntityId(personInfo.getPersonEntityId())
						.description(entity.getDescription())
						.description1(entity.getDescription1())
						.description2(entity.getDescription2())
						.updateTimestamp(entity.getUpdateTimestamp())
						.updateUser(entity.getUpdateUser())
						.entityInfo(personInfo).build());
			}

		}
		return output;
	}

	public List<OPAInstituteResourceUseResponseDTO> savePEComponent(OPAInstituteResourceUseRequestModel opaRequest) {
		if (opaRequest.getOpaInstResId() != null) {
			return updatePEComponent(opaRequest);
		}
		return addPEComponent(opaRequest);
	}

	private List<OPAInstituteResourceUseResponseDTO> addPEComponent(OPAInstituteResourceUseRequestModel opaRequest) {
		
		OPADisclPersonEntity opaPersonEntity = opaCommonDAO.getOPAPersonEntity( opaRequest.getOpaDisclosureId(), 
				opaRequest.getPersonEntityId(),
				opaRequest.getUpdateUser());	
		
		opaRequest.setOpaDisclPersonEntityId(opaPersonEntity.getOpaDisclPersonEntityId());
		List<OPAInstituteResourceUseEntity> lsEntity = InsertPEComponent(opaRequest);
		List<OPAInstituteResourceUseResponseDTO> output = prepareAddPEResponseDTO(opaRequest.getPersonEntityId(), lsEntity);
		
		return output;

	}

	private List<OPAInstituteResourceUseEntity> InsertPEComponent(OPAInstituteResourceUseRequestModel opaRequest) {
		List<OPAInstituteResourceUseEntity> lsEntity = new ArrayList<>();
		OPAInstituteResourceUseEntity entity = mapRequestToEntity(opaRequest);
		hibernateTemplate.saveOrUpdate(entity);
		lsEntity.add(entity);
		return lsEntity;
	}
	
	private List<OPAInstituteResourceUseResponseDTO> prepareAddPEResponseDTO(Integer personEntityId, List<OPAInstituteResourceUseEntity> lsEntity) {

		List<OPAInstituteResourceUseResponseDTO> output = new ArrayList<>();
		
		Map<Integer, OPAPersonEntityInfoDTO> personInfoMap = opaCommonDAO.getPersonEntityInfo(personEntityId);

		for (OPAInstituteResourceUseEntity entity : lsEntity) {
			OPAPersonEntityInfoDTO personInfo = personInfoMap.get(personEntityId);
			if (personInfo != null) {
				output.add(OPAInstituteResourceUseResponseDTO.builder()
						.opaInstResId(entity.getOpaInstResId())
						.opaDisclosureId(entity.getOpaDisclosureId())
						.opaDisclPersonEntityId(entity.getOpaDisclPersonEntityId())
						.personEntityId(personInfo.getPersonEntityId())
						.description(entity.getDescription())
						.description1(entity.getDescription1())
						.description2(entity.getDescription2())
						.updateTimestamp(entity.getUpdateTimestamp())
						.updateUser(entity.getUpdateUser())
						.entityInfo(personInfo).build());
			}

		}

		return output;

	}

	private List<OPAInstituteResourceUseResponseDTO> updatePEComponent(OPAInstituteResourceUseRequestModel opaRequest) {
		OPAInstituteResourceUseEntity entity = mapRequestToEntity(opaRequest);
		try {
			hibernateTemplate.saveOrUpdate(entity);
		} catch (Exception e) {
			throw new RuntimeException("Error OPAInstituteResourceUseComponent - in updatePEComponent() --> " + e.getMessage());
		}
		return prepareResponse(entity, opaRequest);
	}

	private List<OPAInstituteResourceUseResponseDTO> prepareResponse(OPAInstituteResourceUseEntity entity, OPAInstituteResourceUseRequestModel opaRequest) {

		List<OPAInstituteResourceUseResponseDTO> output = new ArrayList<>();

		output.add(OPAInstituteResourceUseResponseDTO.builder()
				.opaInstResId(entity.getOpaInstResId())
				.opaDisclosureId(entity.getOpaDisclosureId())
				.opaDisclPersonEntityId(entity.getOpaDisclPersonEntityId())
				.personEntityId(opaRequest.getEntityInfo().getPersonEntityId())
				.description(entity.getDescription())
				.description1(entity.getDescription1())
				.description2(entity.getDescription2())
				.updateTimestamp(entity.getUpdateTimestamp())
				.updateUser(entity.getUpdateUser())
				.entityInfo(opaRequest.getEntityInfo()).build());

		return output;
	}

	private OPAInstituteResourceUseEntity mapRequestToEntity(OPAInstituteResourceUseRequestModel opaRequest) {
		return OPAInstituteResourceUseEntity.builder()
				.opaInstResId(opaRequest.getOpaInstResId())
				.opaDisclosureId(opaRequest.getOpaDisclosureId())
				.opaDisclPersonEntityId(opaRequest.getOpaDisclPersonEntityId())
				.description(opaRequest.getDescription())
				.description1(opaRequest.getDescription1())
				.description2(opaRequest.getDescription2())
				.updateTimestamp(new Date())
				.updateUser(opaRequest.getUpdateUser())
				.build();
	}

	@Transactional
	public void deletePEComponent(OPAInstituteResourceUseRequestModel opaRequest) {
		OPAInstituteResourceUseEntity entity = mapRequestToEntity(opaRequest);
		if (entity != null) {
			hibernateTemplate.delete(entity);
		}
	}

}
