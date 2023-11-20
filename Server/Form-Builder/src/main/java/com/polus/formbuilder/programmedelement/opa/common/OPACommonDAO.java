package com.polus.formbuilder.programmedelement.opa.common;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Component;

import jakarta.persistence.EntityManager;
import jakarta.persistence.ParameterMode;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;
import jakarta.persistence.StoredProcedureQuery;
import jakarta.transaction.Transactional;
import jakarta.transaction.Transactional.TxType;

@Transactional
@Component
public class OPACommonDAO {

	@PersistenceContext
	private EntityManager entityManager;
	
	@Autowired
	private OPADisclPersonEntityRepository disclPersonEntityRepository;
	
	@Autowired
	private HibernateTemplate hibernateTemplate;

	
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
	@Transactional(value = TxType.SUPPORTS)
	public Map<Integer, OPAPersonEntityInfoDTO> getOPAPersonEntityInfo(Integer opaDisclosureId) {
		try {
			StoredProcedureQuery query = entityManager.createStoredProcedureQuery("GET_OPA_PERSON_ENTITY_DETAILS")
					.registerStoredProcedureParameter(1, Integer.class, ParameterMode.IN);

			query.setParameter(1, opaDisclosureId);
			query.execute();
			List<Object> resultList = query.getResultList();
			Map<Integer, OPAPersonEntityInfoDTO> resultMap = resultList.stream().map(this::convertOPAPersonEntityToDTO)
					.filter(map -> !map.isEmpty()).collect(Collectors.toMap(map -> map.keySet().iterator().next(),
							map -> map.values().iterator().next()));

			return resultMap;

		} catch (Exception e) {
			System.out.println(e.getMessage());
			return new HashMap<>();
		}

	}

	private Map<Integer, OPAPersonEntityInfoDTO> convertOPAPersonEntityToDTO(Object object) {

		Map<Integer, OPAPersonEntityInfoDTO> hmOPAPersnEntity = new HashMap<Integer, OPAPersonEntityInfoDTO>();
		if (object instanceof Object[] data) {
			int i = 0;
			OPAPersonEntityInfoDTO opaPersonEntityInfoDTO = new OPAPersonEntityInfoDTO();

			opaPersonEntityInfoDTO.setOpaDisclPersonEntityId((Integer) data[i++]);
			opaPersonEntityInfoDTO.setPersonEntityId((Integer) data[i++]);
			opaPersonEntityInfoDTO.setPersonId((String) data[i++]);
			opaPersonEntityInfoDTO.setEntityNumber((Integer) data[i++]);
			opaPersonEntityInfoDTO.setEntityName((String) data[i++]);
			opaPersonEntityInfoDTO.setEntityType((String) data[i++]);
			opaPersonEntityInfoDTO.setCountryName((String) data[i++]);
			opaPersonEntityInfoDTO.setRelationship((String) data[i++]);
			opaPersonEntityInfoDTO.setInvolvementStartDate((Date) data[i++]);
			opaPersonEntityInfoDTO.setInvolvementEndDate((Date) data[i++]);
			opaPersonEntityInfoDTO.setEntityStatus((String) data[i++]);
			opaPersonEntityInfoDTO.setIsRelationshipActive((Character) data[i++]);
			opaPersonEntityInfoDTO.setSfiVersionStatus((String) data[i++]);
			opaPersonEntityInfoDTO.setEntityRiskCategory((String) data[i++]);

			hmOPAPersnEntity.put(opaPersonEntityInfoDTO.getOpaDisclPersonEntityId(), opaPersonEntityInfoDTO);
			return hmOPAPersnEntity;

		} else {
			return hmOPAPersnEntity;
		}
	}

	
	@SuppressWarnings("unchecked")
	@Transactional(value =  TxType.SUPPORTS)
	public Map<Integer, OPAPersonEntityInfoDTO> getPersonEntityInfo(Integer personEntityId) {
		try {
			StoredProcedureQuery query = entityManager.createStoredProcedureQuery("GET_PERSON_ENTITY_INFO")
					.registerStoredProcedureParameter(1, Integer.class, ParameterMode.IN);

			query.setParameter(1, personEntityId);
			query.execute();
			
			List<Object> resultList = query.getResultList();
			Map<Integer, OPAPersonEntityInfoDTO> resultMap = resultList.stream().map(this::convertPersonEntityToDTO)
					.filter(map -> !map.isEmpty()).collect(Collectors.toMap(map -> map.keySet().iterator().next(),
							map -> map.values().iterator().next()));

			return resultMap;

		} catch (Exception e) {
			System.out.println(e.getMessage());
			return new HashMap<>();
		}

	}
	

	private Map<Integer, OPAPersonEntityInfoDTO> convertPersonEntityToDTO(Object object) {

		Map<Integer, OPAPersonEntityInfoDTO> hmOPAPersnEntity = new HashMap<Integer, OPAPersonEntityInfoDTO>();
		if (object instanceof Object[] data) {
			int i = 0;
			OPAPersonEntityInfoDTO opaPersonEntityInfoDTO = new OPAPersonEntityInfoDTO();			
			opaPersonEntityInfoDTO.setPersonEntityId((Integer) data[i++]);
			opaPersonEntityInfoDTO.setPersonId((String) data[i++]);
			opaPersonEntityInfoDTO.setEntityNumber((Integer) data[i++]);
			opaPersonEntityInfoDTO.setEntityName((String) data[i++]);
			opaPersonEntityInfoDTO.setEntityType((String) data[i++]);
			opaPersonEntityInfoDTO.setCountryName((String) data[i++]);
			opaPersonEntityInfoDTO.setRelationship((String) data[i++]);
			opaPersonEntityInfoDTO.setInvolvementStartDate((Date) data[i++]);
			opaPersonEntityInfoDTO.setInvolvementEndDate((Date) data[i++]);
			opaPersonEntityInfoDTO.setEntityStatus((String) data[i++]);
			opaPersonEntityInfoDTO.setIsRelationshipActive((Character) data[i++]);
			opaPersonEntityInfoDTO.setSfiVersionStatus((String) data[i++]);
			opaPersonEntityInfoDTO.setEntityRiskCategory((String) data[i++]);
			
			hmOPAPersnEntity.put(opaPersonEntityInfoDTO.getPersonEntityId(), opaPersonEntityInfoDTO);
			
			return hmOPAPersnEntity;

		} else {
			return hmOPAPersnEntity;
		}
	}

	public OPADisclPersonEntity addToOPADisclPersonEntity(Integer opaDisclosureId, Integer personEntityId, String updateUser) {
		OPADisclPersonEntity opaPersonEntity;
		opaPersonEntity = prepareOpaPersonEntity(opaDisclosureId,personEntityId,updateUser);
		hibernateTemplate.saveOrUpdate(opaPersonEntity);
		Integer primaryKey = opaPersonEntity.getOpaDisclPersonEntityId();
		opaPersonEntity.setOpaDisclPersonEntityId(primaryKey);
		return opaPersonEntity;
	}

	
	public OPADisclPersonEntity prepareOpaPersonEntity(Integer opaDisclosureId, Integer personEntityId, String updateUser) {

		OPADisclPersonEntity personEntity = new OPADisclPersonEntity();
		personEntity.setOpaDisclosureId(opaDisclosureId);
		personEntity.setUpdateTimestamp(new Date());
		personEntity.setUpdateUser(updateUser);

		String sql = "SELECT PERSON_ENTITY_ID,ENTITY_ID,ENTITY_NUMBER,PERSON_ENTITY_NUMBER FROM PERSON_ENTITY WHERE PERSON_ENTITY_ID = :personEntityId";
		Query query = entityManager.createNativeQuery(sql);
		query.setParameter("personEntityId", personEntityId);
		List<?> resultRows = query.getResultList();

		for (Object row : resultRows) {
			if (row instanceof Object[]) {
				Object[] rowData = (Object[]) row;
				personEntity.setPersonEntityId((Integer) rowData[0]);
				personEntity.setEntityId((Integer) rowData[1]);
				personEntity.setEntityNumber((Integer) rowData[2]);
				personEntity.setPersonEntityNumber((Integer) rowData[3]);
			}
		}

		return personEntity;
	}

	
	public OPADisclPersonEntity getOPAPersonEntity(Integer opaDisclosureId, Integer personEntityId, String updateUser) {
		OPADisclPersonEntity opaPersonEntity = disclPersonEntityRepository
				.FetchByPersonEntityId(opaDisclosureId, personEntityId);	
		// START - If there is no entry in OPA_DISCL_PERSON_ENTITY table against an PERSON_ENTITY then Sync OPA_DISCL_PERSON_ENTITY table
		if (opaPersonEntity == null) {
			opaPersonEntity = addToOPADisclPersonEntity(opaDisclosureId, 
														personEntityId,
														updateUser);
		}
		// END - If there is no entry in OPA_DISCL_PERSON_ENTITY table against an PERSON_ENTITY then Sync OPA_DISCL_PERSON_ENTITY table
		
		return opaPersonEntity;
	}
}
