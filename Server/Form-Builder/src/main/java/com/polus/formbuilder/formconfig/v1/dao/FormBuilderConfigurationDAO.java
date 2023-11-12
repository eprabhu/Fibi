package com.polus.formbuilder.formconfig.v1.dao;

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
import com.polus.formbuilder.formconfig.v1.model.FormDashboardResponseModel;
import com.polus.formbuilder.formconfig.v1.model.FormDataResponseModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderCreateModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderUpdateModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionComponentModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionModel;
import com.polus.formbuilder.formconfig.v1.model.FormUsageModel;
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
	            .build();
	}


	public FormDataResponseModel fetchFormById(Integer formBuilderId) {
		
		CompletableFuture<Optional<FormBuilderHeaderEntity>> f1 = CompletableFuture.supplyAsync(() -> headerRespository.findById(formBuilderId));
		CompletableFuture<List<FormBuilderUsageEntity>> f2 = CompletableFuture.supplyAsync(() -> usageRepository.fetchByFormId(formBuilderId));
		CompletableFuture<List<FormBuilderSectionEntity>> f3 = CompletableFuture.supplyAsync(() -> sectionRepository.getSectionDetailsByFormId(formBuilderId));
		CompletableFuture<List<FormBuilderSectionComponentEntity>> f4 = CompletableFuture.supplyAsync(() -> componentRepository.fetchByFormId(formBuilderId));
		CompletableFuture<List<FormBuilderProgElementEntity>> f5 = CompletableFuture.supplyAsync(() -> programmedElementRepository.findAll());
		CompletableFuture<List<FormBuilderComponentTypeEntity>> f6 = CompletableFuture.supplyAsync(() -> componentTypeRepository.findAll());
		
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
																	  .map((entity -> mapEntityToModel(entity,formBuilderId)))	
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
																				          .collect(Collectors.groupingBy(FormSectionComponentModel:: getFormBuilderSectionId));
				
		sectionList
			.forEach( section -> section.setSectionComponents(componentBySectionId.get(section.getFormBuilderSectionId())));
		
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
									 .formBuilderSectionId(entity.getFormBuilderSectionId())
									 .formBuilderId(entity.getFormHeaderId())									 
									 .sectionName(entity.getSectionName())
									 .sectionOrderNumber(entity.getSectionOrderNumber())									 
									 .businessRuleId(entity.getBusinessRuleId())
									 .description(entity.getDescription())
									 .helpText(entity.getHelpText())
									 .headerInstruction(entity.getHeaderInstruction())									 
									 .footerInstruction(entity.getFooterInstruction())
							         .isActive(entity.getIsActive())
							         .updateUser(entity.getUpdateUser())
							         .updateTimestamp(entity.getUpdateTimestamp())
									 .build();
	}	

	public FormSectionComponentModel mapEntityToModel(FormBuilderSectionComponentEntity entity, Integer formBuilderId) {

		return FormSectionComponentModel.builder()
									 .formBuilderSectCompId(entity.getFormBuilderSectCompId())
									 .formBuilderSectionId(entity.getSectionId())
									 .formBuilderId(formBuilderId)
									 .componentTypeCode(entity.getComponentTypeCode())
									 .componentOrderNumber(entity.getComponentOrderNumber())									 
									 .componentData(entity.getComponentData())
									 .componentRefId(entity.getComponentRefId())
									 .description(entity.getDescription())
									 .headerInstruction(entity.getHeaderInstruction())									 
									 .footerInstruction(entity.getFooterInstruction())
							         .isActive(entity.getIsActive())
							         .updateUser(entity.getUpdateUser())
							         .updateTimestamp(entity.getUpdateTimestamp())
									 .build();
	}


	private String getLoggedInUser() {		
		try {
			return "admin";//AuthenticatedUser.getLoginUserName();
			
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
		
		
	}catch(Exception e) {
		e.printStackTrace();
	}
	
	return null;
	
}


public FormHeaderModel updateFormHeader(FormHeaderUpdateModel request) {
	try {
		
		String loginUser = getLoggedInUser();
				
		Optional<FormBuilderHeaderEntity> entityOptional
					= headerRespository.findById(request.formHeaderId());
		
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
		
		
	}catch(Exception e) {
		e.printStackTrace();
	}
	
	return null;
}


public String deleteHeader(Integer formBuilderId) {
	Optional<FormBuilderHeaderEntity> entityOptional = headerRespository.findById(formBuilderId);

	if (entityOptional.isEmpty()) {
		return "Requested FormHeaderId not present!!";
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
	
}
