package com.polus.formbuilder.formconfig.v1.dao;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;

import com.polus.appcorelib.authentication.AuthenticatedUser;
import com.polus.formbuilder.entity.FormBuilderComponentTypeEntity;
import com.polus.formbuilder.entity.FormBuilderHeaderEntity;
import com.polus.formbuilder.entity.FormBuilderProgElementEntity;
import com.polus.formbuilder.entity.FormBuilderSectionComponentEntity;
import com.polus.formbuilder.entity.FormBuilderSectionEntity;
import com.polus.formbuilder.entity.FormBuilderUsageEntity;
import com.polus.formbuilder.formconfig.v1.model.FormComponentRequestModel;
import com.polus.formbuilder.formconfig.v1.model.FormDashboardResponseModel;
import com.polus.formbuilder.formconfig.v1.model.FormDataResponseModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderCreateModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderUpdateModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionComponentModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionRequestModel;
import com.polus.formbuilder.formconfig.v1.model.FormUsageModel;
import com.polus.formbuilder.formconfig.v1.model.FormUsageRequestModel;
import com.polus.formbuilder.repository.FormBuilderComponentTypeEntityRepository;
import com.polus.formbuilder.repository.FormBuilderHeaderEntityRepository;
import com.polus.formbuilder.repository.FormBuilderProgElementEntityRepository;
import com.polus.formbuilder.repository.FormBuilderSectionComponentEntityRepository;
import com.polus.formbuilder.repository.FormBuilderSectionEntityRepository;
import com.polus.formbuilder.repository.FormBuilderUsageEntityRepository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.transaction.Transactional;

@Repository
@Transactional
public class FormBuilderConfigurationDAO {

	@Autowired
	FormBuilderHeaderEntityRepository headerRespository;
	
	@Autowired
	private FormBuilderUsageEntityRepository usageRepository;
	
	@Autowired
	private FormBuilderSectionEntityRepository sectionRepository;
	
	@Autowired
	private FormBuilderSectionComponentEntityRepository componentRepository;
	
	@Autowired
	private FormBuilderProgElementEntityRepository programmedElementRepository;
	
	@Autowired
	private FormBuilderComponentTypeEntityRepository componentTypeRepository;
		
	@PersistenceContext
	private EntityManager entityManager;
	
	@Autowired
	private HibernateTemplate hibernateTemplate;

	private static final String ACTIVE_FLAG_DEFAULT = "N";
	
	private static final String SUCCESS = "1";

	
	public List<FormDashboardResponseModel> fetchAllForms() {
		List<FormBuilderHeaderEntity> formList = headerRespository.findAll();	
		return formList.stream()
					   .map(this::mapFormBuilderEntityToResponseModel)
					   .collect(Collectors.toList());
	}

	
	public FormDashboardResponseModel mapFormBuilderEntityToResponseModel(FormBuilderHeaderEntity entity) {
	    return FormDashboardResponseModel.builder()
	            .formBuilderId(entity.getFormBuilderId())
	            .formBuilderNumber(entity.getFormBuilderNumber())
	            .versionNumber(entity.getVersionNumber())
	            .versionStatus(entity.getVersionStatus())
	            .title(entity.getTitle())
	            .description(entity.getDescription())
	            .isActive(entity.getIsActive())
	            .updateUser(entity.getUpdateUser())
	            .updateTimestamp(entity.getUpdateTimestamp())
	            .createUser(entity.getCreateUser())
	            .build();
	}


	public FormDataResponseModel fetchFormById(Integer formBuilderId) {
		
		CompletableFuture<Optional<FormBuilderHeaderEntity>> f1 = CompletableFuture.supplyAsync(() -> headerRespository.findById(formBuilderId));
		CompletableFuture<List<FormBuilderUsageEntity>> f2 = CompletableFuture.supplyAsync(() -> usageRepository.fetchByFormId(formBuilderId));
		CompletableFuture<List<FormBuilderSectionEntity>> f3 = CompletableFuture.supplyAsync(() -> sectionRepository.getSectionDetailsByFormId(formBuilderId));
		CompletableFuture<List<FormBuilderSectionComponentEntity>> f4 = CompletableFuture.supplyAsync(() -> componentRepository.fetchByFormId(formBuilderId));
		CompletableFuture<List<FormBuilderProgElementEntity>> f5 = CompletableFuture.supplyAsync(() -> programmedElementRepository.findAll());
		CompletableFuture<List<FormBuilderComponentTypeEntity>> f6 = CompletableFuture.supplyAsync(() -> componentTypeRepository.getAllActiveComponentTypes());
		
		CompletableFuture<Void> allOf = CompletableFuture.allOf(f1, f2, f3, f4, f5, f6);
		
		try {
			allOf.get(); // Wait for all above tasks to complete
		} catch (InterruptedException | ExecutionException e) {
			e.printStackTrace(); 
		}
		
		var headerEntity = f1.join();
		var usageEntity = f2.join();
		var sectionEntity = f3.join();
		var compnentEntity = f4.join();
		var programmedElementEntity = f5.join();
		var componentTypeEntity = f6.join();
		
		if(headerEntity.isEmpty()) {
			return null;
		}
		
		var headerModel = mapEntityToModel(headerEntity.get());
		
		List<FormUsageModel> usageList = usageEntity.stream()
													.map(this::mapEntityToModel)
													.collect(Collectors.toList());
		
		List<FormSectionModel> sectionList = sectionEntity.stream()
														  .map(this::mapEntityToModel)
														  .collect(Collectors.toList());

		List<FormSectionComponentModel> componentList = compnentEntity.stream()
																	  .map(entity -> mapEntityToModel(entity, formBuilderId))
																	  .collect(Collectors.toList());
		
		sectionList = setComponentToSection(sectionList,componentList);
		
		headerModel.setUsages(usageList);
		headerModel.setSections(sectionList);
		
		
		return FormDataResponseModel.builder()
									.formHeader(headerModel)
									.lookupProgramElements(programmedElementEntity)
									.lookupSectionComponentType(componentTypeEntity)
									.build();
	}
		
	private List<FormSectionModel> setComponentToSection(List<FormSectionModel> sectionList,
														 List<FormSectionComponentModel> componentList) {

		Map<Integer, List<FormSectionComponentModel>> componentBySectionId = componentList.stream()
																				          .collect(Collectors.groupingBy(FormSectionComponentModel:: getSectionId));
				
		sectionList
				.forEach(section -> section.setSectionComponent(
						componentBySectionId.get(section.getSectionId()) == null ? new ArrayList<>() : componentBySectionId.get(section.getSectionId())));
		
		return sectionList;
	}


	@Transactional
	public Integer copyFormById(Integer fromFormBuilderId) {
		
		String loginUser = getLoggedInUser();
		String newFormNumber = getNextFormBuilderNumber();
		
		Integer toFormBuilderId = 
				copyFormHeader(fromFormBuilderId,newFormNumber,loginUser);
		
		copyFormUsage(fromFormBuilderId,toFormBuilderId,loginUser);
		copyFormSectionAndItsComponents(fromFormBuilderId,toFormBuilderId,loginUser);
		
		return toFormBuilderId;
		
	}	
	
	protected void copyFormSectionAndItsComponents(Integer fromFormBuilderId, Integer toFormBuilderId, String loginUser) {
		List<FormBuilderSectionEntity> sectionList = sectionRepository.getSectionDetailsByFormId(fromFormBuilderId);
					
		List<FormBuilderSectionComponentEntity> componentList = componentRepository.fetchByFormId(fromFormBuilderId);
		
		Map<Integer,List<FormBuilderSectionComponentEntity>> componentBySection 
														= componentList.stream()
																	   .collect(Collectors.groupingBy(FormBuilderSectionComponentEntity::getSectionId));
		
		for(FormBuilderSectionEntity entity:sectionList) {
			
			FormBuilderSectionEntity newEntity = new FormBuilderSectionEntity();
			BeanUtils.copyProperties(entity, newEntity, "formBuilderSectionId");
			newEntity.setFormHeaderId(toFormBuilderId);
			newEntity.setUpdateTimestamp(new Date());
			newEntity.setUpdateUser(loginUser);
			
			hibernateTemplate.saveOrUpdate(newEntity);
			
			Integer newSectionId = newEntity.getFormBuilderSectionId();
			
			List<FormBuilderSectionComponentEntity> componentsForAsection = componentBySection.get(entity.getFormBuilderSectionId());
			
			for(FormBuilderSectionComponentEntity compEntity : componentsForAsection) {
				
				FormBuilderSectionComponentEntity newCompEntity = new FormBuilderSectionComponentEntity();
				BeanUtils.copyProperties(compEntity, newCompEntity, "formBuilderSectCompId");
				newCompEntity.setSectionId(newSectionId);
				newCompEntity.setUpdateTimestamp(new Date());
				newCompEntity.setUpdateUser(loginUser);
				
				hibernateTemplate.saveOrUpdate(newCompEntity);
				
			}
			
		}
		
	}


	protected void copyFormUsage(Integer fromFormBuilderId, Integer toFormBuilderId,String loginUser) {
		List<FormBuilderUsageEntity> fromUsageLs = usageRepository.fetchByFormId(fromFormBuilderId);
		for(FormBuilderUsageEntity entity : fromUsageLs) {
			try {
				FormBuilderUsageEntity newEntity = new FormBuilderUsageEntity();
				BeanUtils.copyProperties(entity, newEntity, "formUsageId");
				newEntity.setFormBuilderId(toFormBuilderId);
				newEntity.setUpdateTimestamp(new Date());
				newEntity.setCreateTimestamp(new Date());			
				newEntity.setCreateUser(loginUser);
				newEntity.setUpdateUser(loginUser);
				newEntity.setIsActive(ACTIVE_FLAG_DEFAULT);
				
				hibernateTemplate.saveOrUpdate(newEntity);
			}catch(Exception e) {
				e.printStackTrace();
			}
		}
	
	}


	protected Integer copyFormHeader(Integer fromFormBuilderId,String newFormNumber, String loginUser) {
		
		Optional<FormBuilderHeaderEntity> headerEntOptional = headerRespository.findById(fromFormBuilderId);
		if(headerEntOptional.isEmpty()) {
			return null;
		}
		Integer newFormBuilderId = null;
		FormBuilderHeaderEntity headerEntity = headerEntOptional.get();
		try {
			
			FormBuilderHeaderEntity newHeaderEntity = new FormBuilderHeaderEntity();
			BeanUtils.copyProperties(headerEntity, newHeaderEntity, "formBuilderId");
			newHeaderEntity.setUpdateTimestamp(new Date());
			newHeaderEntity.setCreateTimestamp(new Date());
			
			newHeaderEntity.setCreateUser(loginUser);
			newHeaderEntity.setUpdateUser(loginUser);
			newHeaderEntity.setIsActive(ACTIVE_FLAG_DEFAULT);
			newHeaderEntity.setFormBuilderNumber(newFormNumber);
			hibernateTemplate.saveOrUpdate(newHeaderEntity);
			newFormBuilderId = newHeaderEntity.getFormBuilderId();
			return newFormBuilderId;
			
		}catch(Exception e) {
			e.printStackTrace();
		}
		
		return newFormBuilderId;
	}
	
	public FormHeaderModel mapEntityToModel(FormBuilderHeaderEntity entity) {
		return FormHeaderModel.builder()
									 .formBuilderId(entity.getFormBuilderId())
									 .formBuilderNumber(entity.getFormBuilderNumber())
									 .versionNumber(entity.getVersionNumber())
									 .versionStatus(entity.getVersionStatus())									 
									 .title(entity.getTitle())
							         .description(entity.getDescription())
							         .isActive(entity.getIsActive())
							         .createTimestamp(entity.getCreateTimestamp())
							         .createUser(entity.getCreateUser())
							         .updateUser(entity.getUpdateUser())
							         .updateTimestamp(entity.getUpdateTimestamp())
									 .build();
	}
	
	public FormUsageModel mapEntityToModel(FormBuilderUsageEntity entity) {

		return FormUsageModel.builder()
									 .formUsageId(entity.getFormUsageId())
									 .formBuilderId(entity.getFormBuilderId())	
									 .formOrderNumber(entity.getFormOrderNumber())
									 .moduleCode(entity.getModuleCode())
									 .subModuleCode(entity.getSubModuleCode())									 
									 .businessRuleId(entity.getBusinessRuleId())
							         .description(entity.getDescription())
							         .isActive(entity.getIsActive())
							         .createTimestamp(entity.getCreateTimestamp())
							         .createUser(entity.getCreateUser())
							         .updateUser(entity.getUpdateUser())
							         .updateTimestamp(entity.getUpdateTimestamp())
									 .build();
	}

	public FormSectionModel mapEntityToModel(FormBuilderSectionEntity entity) {

		return FormSectionModel.builder()
									 .sectionId(entity.getFormBuilderSectionId())
									 .formBuilderId(entity.getFormHeaderId())									 
									 .sectionName(entity.getSectionName())
									 .sectionOrder(entity.getSectionOrderNumber())									 
									 .sectionBusinessRule(entity.getBusinessRuleId())
									 .sectionDescription(entity.getDescription())
									 .sectionHelpText(entity.getHelpText())
									 .sectionHeader(entity.getHeaderInstruction())									 
									 .sectionFooter(entity.getFooterInstruction())
							         .isActive(entity.getIsActive())
							         .updateUser(entity.getUpdateUser())
							         .updateTimestamp(entity.getUpdateTimestamp())
									 .build();
	}	

	public FormSectionComponentModel mapEntityToModel(FormBuilderSectionComponentEntity entity, Integer formBuilderId) {

		return FormSectionComponentModel.builder()
									 .componentId(entity.getFormBuilderSectCompId())
									 .sectionId(entity.getSectionId())
									 .formBuilderId(formBuilderId)
									 .componentType(entity.getComponentTypeCode())
									 .componentOrder(entity.getComponentOrderNumber())									 
									 .componentData(entity.getComponentData())
									 .componentRefId(entity.getComponentRefId())
									 .description(entity.getDescription())
									 .componentHeader(entity.getHeaderInstruction())									 
									 .componentFooter(entity.getFooterInstruction())
							         .isActive(entity.getIsActive())
							         .updateUser(entity.getUpdateUser())
							         .updateTimestamp(entity.getUpdateTimestamp())
							         .isMandatory(entity.getIsMandatory())
							         .validationMessage(entity.getValidationMessage())
							         .label(entity.getLabel())
									 .build();
	}


	private String getLoggedInUser() {		
		try {
			return AuthenticatedUser.getLoginUserName();
		}catch(Exception e) {
			return "nouser";
		}
	}
	
	@Transactional
	private String getNextFormBuilderNumber() {
        return hibernateTemplate.execute(session -> {
            return session.doReturningWork(connection -> {
                try (java.sql.CallableStatement statement = connection.prepareCall("{call GENERATE_FORM_BUILDER_NUMBER(?)}")) {
                     
                	statement.registerOutParameter(1, java.sql.Types.VARCHAR);
                    statement.execute();
                    String nextValue = statement.getString(1);                   
                    return nextValue;
                }
            });
        });
	}

	@SuppressWarnings("deprecation")
	@Transactional
	public String deleteForm(Integer formBuilderId) {
        hibernateTemplate.execute(session -> {
            try {
                String component = "DELETE FROM FORM_BUILDER_SECTION_COMPONENT WHERE FORM_BUILDER_SECTION_ID IN ( SELECT FORM_BUILDER_SECTION_ID FROM FORM_BUILDER_SECTION WHERE FORM_BUILDER_ID = :formId)";
                session.createNativeQuery(component).setParameter("formId", formBuilderId).executeUpdate();

                String section = "DELETE FROM FORM_BUILDER_SECTION WHERE FORM_BUILDER_ID = :formId";
                session.createNativeQuery(section).setParameter("formId", formBuilderId).executeUpdate();

                String usage = "DELETE FROM FORM_BUILDER_USAGE WHERE FORM_BUILDER_ID = :formId";
                session.createNativeQuery(usage).setParameter("formId", formBuilderId).executeUpdate();

                String header = "DELETE FROM FORM_BUILDER_HEADER WHERE FORM_BUILDER_ID = :formId";
                session.createNativeQuery(header).setParameter("formId", formBuilderId).executeUpdate();

            } catch (Exception e) {
                e.printStackTrace();
                return e.getMessage();
            }
            return SUCCESS; // the return value is required for the execute method
        });
        
        return SUCCESS;
	}
	

public FormHeaderModel createFormHeader(FormHeaderCreateModel request) {
	try {
		String loginUser = getLoggedInUser();
		String newFormNumber = getNextFormBuilderNumber();
		
		FormBuilderHeaderEntity entity = new FormBuilderHeaderEntity();
		entity.setFormBuilderNumber(newFormNumber);
		entity.setVersionNumber(1);
		entity.setVersionStatus("ACTIVE");
		entity.setTitle(request.title());
		entity.setDescription(request.description());
		entity.setUpdateTimestamp(new Date());
		entity.setCreateTimestamp(new Date());		
		entity.setCreateUser(loginUser);
		entity.setUpdateUser(loginUser);
		entity.setIsActive(ACTIVE_FLAG_DEFAULT);	
		
		hibernateTemplate.saveOrUpdate(entity);
		
		return mapEntityToModel(entity);
		
		
	}catch(Exception e) {
		e.printStackTrace();
	}
	
	return null;
	
}


public FormHeaderModel updateFormHeader(FormHeaderUpdateModel request) {
	try {
		
		String loginUser = getLoggedInUser();
				
		Optional<FormBuilderHeaderEntity> entityOptional
					= headerRespository.findById(request.formBuilderId());
		
		if(entityOptional.isEmpty()) {
			return new FormHeaderModel();
		}
		
		FormBuilderHeaderEntity entity = entityOptional.get();		
		entity.setTitle(request.title());
		entity.setDescription(request.description());
		entity.setUpdateTimestamp(new Date());
		entity.setUpdateUser(loginUser);
		entity.setIsActive(request.isActive());
		
		hibernateTemplate.saveOrUpdate(entity);
				
		return mapEntityToModel(entity);
		
		
	}catch(Exception e) {
		e.printStackTrace();
		return new FormHeaderModel();
	}
	
}


public String deleteHeader(Integer formBuilderId) {
	Optional<FormBuilderHeaderEntity> entityOptional = headerRespository.findById(formBuilderId);

	if (entityOptional.isEmpty()) {
		return "Requested Header Id not present!!";
	}

	try {		
		FormBuilderHeaderEntity entity = entityOptional.get();	
		hibernateTemplate.delete(entity);
		
	}catch(Exception e) {
		e.printStackTrace();
		return e.getMessage();
	}	
	
	return SUCCESS;
}


public FormHeaderModel fetchFormHeader(Integer formBuilderId) {
	try {

		Optional<FormBuilderHeaderEntity> entityOptional = headerRespository.findById(formBuilderId);

		if (entityOptional.isEmpty()) {
			return new FormHeaderModel();
		}

		FormBuilderHeaderEntity entity = entityOptional.get();

		return mapEntityToModel(entity);

	} catch (Exception e) {
		e.printStackTrace();
		return new FormHeaderModel();
	}

}

public FormUsageModel createFormUsage(FormUsageRequestModel request) {
	try {
		String loginUser = getLoggedInUser();
		FormBuilderUsageEntity newEntity = new FormBuilderUsageEntity();		
		newEntity.setFormBuilderId(request.getFormBuilderId());
		newEntity.setFormBuilderNumber(request.getFormBuilderNumber());
		newEntity.setFormOrderNumber(fetchFormOrderNumber(request.getModuleCode(), request.getSubModuleCode()));
		newEntity.setModuleCode(request.getModuleCode());
		newEntity.setSubModuleCode(request.getSubModuleCode());
		newEntity.setBusinessRuleId(request.getBusinessRuleId());
		newEntity.setDescription(request.getDescription());
		newEntity.setUpdateTimestamp(new Date());
		newEntity.setCreateTimestamp(new Date());
		newEntity.setCreateUser(loginUser);
		newEntity.setUpdateUser(loginUser);
		newEntity.setIsActive(request.getIsActive());
		hibernateTemplate.saveOrUpdate(newEntity);

		return mapEntityToModel(newEntity);

	} catch (Exception e) {
		e.printStackTrace();
		return new FormUsageModel();
	}

}

private int fetchFormOrderNumber(String moduleCode, String subModuleCode) {
	Integer maxFormOrderNumber = usageRepository.fetchFormOrderNumber(moduleCode, subModuleCode);
	return (maxFormOrderNumber == null ? 1 : (maxFormOrderNumber + 1));
}

public FormUsageModel updateFormUsage(FormUsageRequestModel request) {
	try {

		String loginUser = getLoggedInUser();

		Optional<FormBuilderUsageEntity> entityOptional = usageRepository.findById(request.getFormUsageId());

		if (entityOptional.isEmpty()) {
			return new FormUsageModel();
		}

		FormBuilderUsageEntity entity = entityOptional.get();

		entity.setFormOrderNumber(request.getFormOrderNumber());
		entity.setModuleCode(request.getModuleCode());
		entity.setSubModuleCode(request.getSubModuleCode());
		entity.setBusinessRuleId(request.getBusinessRuleId());
		entity.setDescription(request.getDescription());
		entity.setUpdateTimestamp(new Date());
		entity.setUpdateUser(loginUser);
		entity.setIsActive(request.getIsActive());

		hibernateTemplate.saveOrUpdate(entity);

		return mapEntityToModel(entity);

	} catch (Exception e) {
		e.printStackTrace();
		return new FormUsageModel();
	}
}


public String deleteFormUsage(Integer formUsageId) {
	Optional<FormBuilderUsageEntity> entityOptional = usageRepository.findById(formUsageId);

	if (entityOptional.isEmpty()) {
		return "Requested Usage Id not present!!";
	}

	try {
		FormBuilderUsageEntity entity = entityOptional.get();
		hibernateTemplate.delete(entity);

	} catch (Exception e) {
		e.printStackTrace();
		return e.getMessage();
	}

	return SUCCESS;
}


public List<FormUsageModel> fetchFormUsage(Integer formBuilderId) {
	List<FormBuilderUsageEntity> usageEntityList = usageRepository.fetchByFormId(formBuilderId);
	List<FormUsageModel> usageList = usageEntityList
												.stream()
												.map(this::mapEntityToModel)
												.collect(Collectors.toList());
	return usageList;

}


@SuppressWarnings("deprecation")
public String updateUsageOrder(List<FormUsageRequestModel> request) {
	try {
			 //intentionally commented to update timestamp and updateuser
			 // Also, used native SQL for performance
			//String loginUser = getLoggedInUser();
	
			for(FormUsageRequestModel req: request) {
				
				hibernateTemplate.execute(session -> {
		            try {
		                String usage = " UPDATE FORM_BUILDER_USAGE  SET FORM_ORDER_NUMBER = :orderNumber  "
		                				 + " WHERE FORM_USAGE_ID = :usageId "
		                				 + " AND FORM_ORDER_NUMBER <> :orderNumber";
		                session.createNativeQuery(usage)
		                		.setParameter("usageId", req.getFormUsageId())
		                		.setParameter("orderNumber", req.getFormOrderNumber())
		                		.executeUpdate();
	
		            } catch (Exception e) {
		                e.printStackTrace();
		                return e.getMessage();
		            }
		            return SUCCESS; // the return value is required for the execute method
		        });				
				
			}		
		

		} catch (Exception e) {
			e.printStackTrace();
			return e.getMessage();
		}
	 return SUCCESS; 
	 
}


public List<FormSectionModel> fetchAllFormSection(Integer formBuilderId) {
	List<FormBuilderSectionEntity> sectionEntity = sectionRepository.getSectionDetailsByFormId(formBuilderId);
	List<FormSectionModel> sectionList = sectionEntity.stream()
													  .map(this::mapEntityToModel)
													  .collect(Collectors.toList());
	return sectionList;
}


public FormSectionModel createFormSection(FormSectionRequestModel request) {
	try {
		String loginUser = getLoggedInUser();
		
		FormBuilderSectionEntity newEntity = new FormBuilderSectionEntity();	
		
		newEntity.setFormHeaderId(request.getFormBuilderId());								 
		newEntity.setSectionName(request.getSectionName());
		newEntity.setSectionOrderNumber(request.getSectionOrder());									 
		newEntity.setBusinessRuleId(request.getSectionBusinessRule());
		newEntity.setDescription(request.getSectionDescription());
		newEntity.setHelpText(request.getSectionHelpText());
		newEntity.setHeaderInstruction(request.getSectionHeader());									 
		newEntity.setFooterInstruction(request.getSectionFooter());
		newEntity.setIsActive(request.getIsActive());
		newEntity.setUpdateUser(loginUser);
		newEntity.setUpdateTimestamp(new Date());
		
		hibernateTemplate.saveOrUpdate(newEntity);

		return mapEntityToModel(newEntity);

	} catch (Exception e) {
		e.printStackTrace();
		return new FormSectionModel();
	}

}


public FormSectionModel updateFormSection(FormSectionRequestModel request) {
	try {

		String loginUser = getLoggedInUser();

		Optional<FormBuilderSectionEntity> entityOptional = sectionRepository.findById(request.getSectionId());

		if (entityOptional.isEmpty()) {
			return new FormSectionModel();
		}

		FormBuilderSectionEntity entity = entityOptional.get();

		entity.setSectionName(request.getSectionName());
		entity.setSectionOrderNumber(request.getSectionOrder());									 
		entity.setBusinessRuleId(request.getSectionBusinessRule());
		entity.setDescription(request.getSectionDescription());
		entity.setHelpText(request.getSectionHelpText());
		entity.setHeaderInstruction(request.getSectionHeader());									 
		entity.setFooterInstruction(request.getSectionFooter());
		entity.setIsActive(request.getIsActive());
		entity.setUpdateUser(loginUser);
		entity.setUpdateTimestamp(new Date());
		
		hibernateTemplate.saveOrUpdate(entity);

		return mapEntityToModel(entity);

	} catch (Exception e) {
		e.printStackTrace();
		return new FormSectionModel();
	}
}


public String updateSectionOrder(List<FormSectionRequestModel> request) {
	try {
		 //intentionally commented to update timestamp and updateuser
		 // Also, used native SQL for performance
		//String loginUser = getLoggedInUser();

		for(FormSectionRequestModel req: request) {
			
			hibernateTemplate.execute(session -> {
	            try {
	                String section = " UPDATE FORM_BUILDER_SECTION  SET SECTION_ORDER_NUMBER = :orderNumber  "
	                				 + " WHERE FORM_BUILDER_SECTION_ID = :sectionId "
	                				 + " AND SECTION_ORDER_NUMBER <> :orderNumber";
	                session.createNativeQuery(section)
	                		.setParameter("sectionId", req.getSectionId())
	                		.setParameter("orderNumber", req.getSectionOrder())
	                		.executeUpdate();

	            } catch (Exception e) {
	                e.printStackTrace();
	                return e.getMessage();
	            }
	            return SUCCESS; // the return value is required for the execute method
	        });				
			
		}		
	

	} catch (Exception e) {
		e.printStackTrace();
		return e.getMessage();
	}
return SUCCESS; 

}


public String deleteFormSection(int formBuilderSectionId) {
	Optional<FormBuilderSectionEntity> entityOptional = sectionRepository.findById(formBuilderSectionId);

	if (entityOptional.isEmpty()) {
		return "Requested Section Id not present!!";
	}

	deleteFormComponentBySectionId(formBuilderSectionId);

	try {
		FormBuilderSectionEntity entity = entityOptional.get();
		hibernateTemplate.delete(entity);

	} catch (Exception e) {
		e.printStackTrace();
		return e.getMessage();
	}

	return SUCCESS;
}


public FormSectionModel fetchFormSection(Integer formBuilderSectionId) {
	try {

		Optional<FormBuilderSectionEntity> entityOptional = sectionRepository.findById(formBuilderSectionId);

		if (entityOptional.isEmpty()) {
			return new FormSectionModel();
		}

		FormBuilderSectionEntity entity = entityOptional.get();
		return mapEntityToModel(entity);

	} catch (Exception e) {
		e.printStackTrace();
		return new FormSectionModel();
	}
}


public List<FormSectionComponentModel> fetchAllFormComponent(Integer formBuilderSectionId) {
	
	try {

		Optional<FormBuilderSectionEntity> entityOptional = sectionRepository.findById(formBuilderSectionId);

		if (entityOptional.isEmpty()) {
			return new ArrayList<FormSectionComponentModel>();
		}

		FormBuilderSectionEntity section = entityOptional.get();
		
		List<FormBuilderSectionComponentEntity> compList =  componentRepository.getAllComponentBySection(formBuilderSectionId);
		List<FormSectionComponentModel> componentList
												= compList.stream()
				  										  .map((entity -> mapEntityToModel(entity,section.getFormHeaderId())))	
			  											  .collect(Collectors.toList());
		return componentList;
	
	} catch (Exception e) {
		e.printStackTrace();
		return new ArrayList<FormSectionComponentModel>();
	}
}


public FormSectionComponentModel fetchFormComponent(Integer formBuilderSectCompId) {
	Optional<FormBuilderSectionComponentEntity> compOptional =  componentRepository.findById(formBuilderSectCompId);
	
	if (compOptional.isEmpty()) {
		return new FormSectionComponentModel();
	}
	FormBuilderSectionComponentEntity compEntity = compOptional.get();
	
	return mapEntityToModel(compEntity,null);
}


public FormSectionComponentModel createFormComponent(FormComponentRequestModel request) {
	try {
		String loginUser = getLoggedInUser();
		
		FormBuilderSectionComponentEntity newEntity = new FormBuilderSectionComponentEntity();	
		
		newEntity.setSectionId(request.getSectionId());
		newEntity.setComponentTypeCode(request.getComponentType());
		newEntity.setComponentOrderNumber(request.getComponentOrder());									 
		newEntity.setComponentData(request.getComponentData());
		newEntity.setComponentRefId(request.getComponentRefId());
		newEntity.setDescription(request.getDescription());
		newEntity.setHeaderInstruction(request.getComponentHeader());								 
		newEntity.setFooterInstruction(request.getComponentFooter());
		newEntity.setIsActive(request.getIsActive());
		newEntity.setUpdateUser(loginUser);
		newEntity.setUpdateTimestamp(new Date());
		newEntity.setIsMandatory(request.getIsMandatory());
		newEntity.setValidationMessage(request.getValidationMessage());
		newEntity.setLabel(request.getLabel());
						
		hibernateTemplate.saveOrUpdate(newEntity);

		FormBuilderSectionComponentEntity entity = new FormBuilderSectionComponentEntity();
		BeanUtils.copyProperties(newEntity,entity);

		return mapEntityToModel(entity,null);

	} catch (Exception e) {
		e.printStackTrace();
		return new FormSectionComponentModel();
	}
	
}


public String deleteFormComponent(int formBuilderSectCompId) {
	Optional<FormBuilderSectionComponentEntity> entityOptional = componentRepository.findById(formBuilderSectCompId);

	if (entityOptional.isEmpty()) {
		return "Requested Component Id not present!!";
	}

	try {
		FormBuilderSectionComponentEntity entity = entityOptional.get();
		hibernateTemplate.delete(entity);

	} catch (Exception e) {
		e.printStackTrace();
		return e.getMessage();
	}

	return SUCCESS;
}


public FormSectionComponentModel updateFormComponent(FormComponentRequestModel request) {
	try {

		String loginUser = getLoggedInUser();

		Optional<FormBuilderSectionComponentEntity> entityOptional = componentRepository.findById(request.getComponentId());

		if (entityOptional.isEmpty()) {
			return new FormSectionComponentModel();
		}

		FormBuilderSectionComponentEntity entity = entityOptional.get();

		entity.setComponentTypeCode(request.getComponentType());
		entity.setComponentOrderNumber(entity.getComponentOrderNumber() != null ? entity.getComponentOrderNumber() : request.getComponentOrder());
		entity.setComponentData(request.getComponentData());
		entity.setComponentRefId(request.getComponentRefId());
		entity.setDescription(request.getDescription());
		entity.setHeaderInstruction(request.getComponentHeader());								 
		entity.setFooterInstruction(request.getComponentFooter());
		entity.setIsActive(request.getIsActive());
		entity.setUpdateUser(loginUser);
		entity.setUpdateTimestamp(new Date());
		entity.setIsMandatory(request.getIsMandatory());
		entity.setValidationMessage(request.getValidationMessage());
		entity.setLabel(request.getLabel());
		
		hibernateTemplate.saveOrUpdate(entity);

		return mapEntityToModel(entity,null);

	} catch (Exception e) {
		e.printStackTrace();
		return new FormSectionComponentModel();
	}
}


@SuppressWarnings("deprecation")
public String updateComponentOrder(List<FormComponentRequestModel> request) {
	try {
		// intentionally commented to update timestamp and updateuser
		// Also, used native SQL for performance
		// String loginUser = getLoggedInUser();

		for (FormComponentRequestModel req : request) {

			hibernateTemplate.execute(session -> {
				try {
					String component = " UPDATE FORM_BUILDER_SECTION_COMPONENT SET COMPONENT_ORDER_NUMBER = :orderNumber, "
							+ "  FORM_BUILDER_SECTION_ID = :sectionId  "
							+ " WHERE FORM_BUILDER_SECT_COMP_ID = :componentId "
							+ " AND ( COMPONENT_ORDER_NUMBER <> :orderNumber "
							+ " OR FORM_BUILDER_SECTION_ID <> :sectionId)";
					session.createNativeQuery(component).setParameter("componentId", req.getComponentId())
							.setParameter("orderNumber", req.getComponentOrder())
							.setParameter("sectionId", req.getSectionId()).executeUpdate();

				} catch (Exception e) {
					e.printStackTrace();
					return e.getMessage();
				}
				return SUCCESS; // the return value is required for the execute method
			});

		}

	} catch (Exception e) {
		e.printStackTrace();
		return e.getMessage();
	}
	return SUCCESS;
}

@SuppressWarnings("deprecation")
public String deleteFormComponentBySectionId(int formBuilderSectId) {
	return hibernateTemplate.execute(session -> {
        try {
            String component = "DELETE FROM FORM_BUILDER_SECTION_COMPONENT WHERE FORM_BUILDER_SECTION_ID = :formBuilderSectId";
            session.createNativeQuery(component).setParameter("formBuilderSectId", formBuilderSectId).executeUpdate();
        } catch (Exception e) {
            e.printStackTrace();
            return e.getMessage();
        }
        return SUCCESS;
    });
    
}

}
