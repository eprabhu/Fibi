package com.polus.formbuilder.formconfig;

import static org.hamcrest.Matchers.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.formbuilder.formconfig.v1.controller.FormBuilderConfigurationController;
import com.polus.formbuilder.formconfig.v1.model.FormBasicCommonModel;
import com.polus.formbuilder.formconfig.v1.model.FormComponentRequestModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderCreateModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderUpdateModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionRequestModel;
import com.polus.formbuilder.formconfig.v1.model.FormUsageRequestModel;

@WebMvcTest(FormBuilderConfigurationController.class)
public class FormConfigControllerTest {
	@Autowired
	private MockMvc mockMvc;

	@Autowired
	private ObjectMapper objectMapper;

//COPY and DELETE FORM

	@Test
	void formCopyandDelete() throws Exception {

		int fromFormBuilderId = 2;
		FormBasicCommonModel copyRequest = new FormBasicCommonModel(fromFormBuilderId);
		String copyrequestJson = objectMapper.writeValueAsString(copyRequest);

		MvcResult result = mockMvc
				.perform(post("/config/v1/copyform").contentType(MediaType.APPLICATION_JSON).content(copyrequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.formHeader.description", is("Staff OPA Form")))
				.andReturn();

		String jsonResponse = result.getResponse().getContentAsString();

		//System.out.println(jsonResponse);
		JsonNode jsonNode = objectMapper.readTree(jsonResponse);

		String formBuilderId = jsonNode.path("formHeader").path("formBuilderId").asText();

		FormBasicCommonModel delRequest = new FormBasicCommonModel(Integer.parseInt(formBuilderId));
		String delrequestJson = objectMapper.writeValueAsString(delRequest);

		mockMvc.perform(delete("/config/v1/deleteform").contentType(MediaType.APPLICATION_JSON).content(delrequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.message", is("1"))).andReturn();

	}

// HEADER ---------------------------------------------------

	@Test
	void formHeaderTestCases() throws Exception {

		// CREATE
		FormHeaderCreateModel request = new FormHeaderCreateModel("test Title", "test description");
		String requestJson = objectMapper.writeValueAsString(request);

		MvcResult result = mockMvc
				.perform(post("/config/v1/formheader").contentType(MediaType.APPLICATION_JSON).content(requestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.title", is("test Title")))
				.andExpect(jsonPath("$.description", is("test description"))).andReturn();

		String jsonResponse = result.getResponse().getContentAsString();
		JsonNode jsonNode = objectMapper.readTree(jsonResponse);
		String formBuilderId = jsonNode.path("formBuilderId").asText();

		// UPDATE
		FormHeaderUpdateModel updRequest = new FormHeaderUpdateModel(Integer.parseInt(formBuilderId),
				"updated test Title", "test description", "Y");
		String updRequestJson = objectMapper.writeValueAsString(updRequest);
		mockMvc.perform(put("/config/v1/formheader").contentType(MediaType.APPLICATION_JSON).content(updRequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.title", is("updated test Title")))
				.andExpect(jsonPath("$.description", is("test description"))).andReturn();

		// GET
		mockMvc.perform(get("/config/v1/formheader/{formBuilderId}", Integer.parseInt(formBuilderId)))
				.andExpect(status().isOk()).andExpect(jsonPath("$.title", is("updated test Title")))
				.andExpect(jsonPath("$.description", is("test description"))).andReturn();

		// DELETE
		FormBasicCommonModel delRequest = new FormBasicCommonModel(Integer.parseInt(formBuilderId));
		String delrequestJson = objectMapper.writeValueAsString(delRequest);

		mockMvc.perform(delete("/config/v1/formheader").contentType(MediaType.APPLICATION_JSON).content(delrequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.message", is("1"))).andReturn();

	}

// USAGE -----------------------------------------

	@Test
	void formUsageTestCases() throws Exception {

		FormUsageRequestModel model = new FormUsageRequestModel();
		model.setFormBuilderId(10);
		model.setFormOrderNumber(1026);
		model.setModuleCode("23");
		model.setSubModuleCode("0");
		model.setBusinessRuleId(9990);
		model.setDescription("Sample Description");
		model.setIsActive("Y");

		String requestJson = objectMapper.writeValueAsString(model);

		MvcResult result = mockMvc
				.perform(post("/config/v1/formusage").contentType(MediaType.APPLICATION_JSON).content(requestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.description", is("Sample Description"))).andReturn();

		String jsonResponse = result.getResponse().getContentAsString();
		JsonNode jsonNode = objectMapper.readTree(jsonResponse);
		String formUsageId = jsonNode.path("formUsageId").asText();

		// UPDATE -- USAGE

		FormUsageRequestModel updRequest = new FormUsageRequestModel(Integer.parseInt(formUsageId), 10, "10", 1, "23", "0",
				9990, "updated usage description", "Y");
		String updRequestJson = objectMapper.writeValueAsString(updRequest);
		mockMvc.perform(put("/config/v1/formusage").contentType(MediaType.APPLICATION_JSON).content(updRequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.description", is("updated usage description")))
				.andReturn();

		// GET - USAGE
		mockMvc.perform(get("/config/v1/formusage/{formBuilderId}", 10)).andExpect(status().isOk())
				.andExpect(jsonPath("$.[0].description", is("updated usage description"))).andReturn();

		// PATCH - USAGE
		FormUsageRequestModel patchRequest = new FormUsageRequestModel(Integer.parseInt(formUsageId), 10, "10", 5, null, null,
				null, null, null);
		List<FormUsageRequestModel> patchRequestls = List.of(patchRequest);

		String patchRequestJson = objectMapper.writeValueAsString(patchRequestls);

		mockMvc.perform(
				patch("/config/v1/formusage/order").contentType(MediaType.APPLICATION_JSON).content(patchRequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.message", is("1"))).andReturn();

		// DELETE

		FormUsageRequestModel delRequest = new FormUsageRequestModel(Integer.parseInt(formUsageId), 0, "0", 0, null, null,
				null, null, null);
		String delrequestJson = objectMapper.writeValueAsString(delRequest);

		mockMvc.perform(delete("/config/v1/formusage").contentType(MediaType.APPLICATION_JSON).content(delrequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.message", is("1"))).andReturn();

	}

// SECTION		
	@Test
	void formSectionTestCases() throws Exception {

		FormSectionRequestModel model = new FormSectionRequestModel();
		model.setFormBuilderId(10);
		model.setSectionName("New Test Section");
		model.setSectionOrder(1);
		model.setSectionHelpText("Help Text for Test");
		model.setSectionHeader("Test Header");
		model.setSectionFooter("Test Footer");
		model.setSectionBusinessRule(null);
		model.setSectionDescription("Sample Description");
		model.setIsActive("Y");

		String requestJson = objectMapper.writeValueAsString(model);

		MvcResult result = mockMvc
				.perform(post("/config/v1/formsection").contentType(MediaType.APPLICATION_JSON).content(requestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.description", is("Sample Description")))
				.andExpect(jsonPath("$.helpText", is("Help Text for Test"))).andReturn();

		String jsonResponse = result.getResponse().getContentAsString();
		JsonNode jsonNode = objectMapper.readTree(jsonResponse);
		String formSectionId = jsonNode.path("formBuilderSectionId").asText();

		// UPDATE -- SECTION

		FormSectionRequestModel updatemodel = new FormSectionRequestModel();
		updatemodel.setSectionId(Integer.parseInt(formSectionId));
		updatemodel.setFormBuilderId(10);
		updatemodel.setSectionName("New Test Section");
		updatemodel.setSectionOrder(1);
		updatemodel.setSectionHelpText("Updated Help Text for Test");
		updatemodel.setSectionHeader("Test Header");
		updatemodel.setSectionFooter("Test Footer");
		updatemodel.setSectionBusinessRule(null);
		updatemodel.setSectionDescription("Updated Sample Description");
		updatemodel.setIsActive("Y");

		String updRequestJson = objectMapper.writeValueAsString(updatemodel);

		mockMvc.perform(put("/config/v1/formsection").contentType(MediaType.APPLICATION_JSON).content(updRequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.description", is("Updated Sample Description")))
				.andExpect(jsonPath("$.helpText", is("Updated Help Text for Test"))).andReturn();

		// GET - SECTION

		mockMvc.perform(get("/config/v1/formsection/{formBuilderSectionId}", Integer.parseInt(formSectionId)))
				.andExpect(status().isOk()).andExpect(jsonPath("$.description", is("Updated Sample Description")))
				.andReturn();

		// PATCH - SECTION
		FormSectionRequestModel patchRequest = new FormSectionRequestModel();
		patchRequest.setSectionId(Integer.parseInt(formSectionId));
		patchRequest.setFormBuilderId(10);
		patchRequest.setSectionOrder(1);
		List<FormSectionRequestModel> patchRequestls = List.of(patchRequest);

		String patchRequestJson = objectMapper.writeValueAsString(patchRequestls);

		mockMvc.perform(
				patch("/config/v1/formsection/order").contentType(MediaType.APPLICATION_JSON).content(patchRequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.message", is("1"))).andReturn();

		// DELETE

		FormSectionRequestModel delRequest = new FormSectionRequestModel();
		delRequest.setSectionId(Integer.parseInt(formSectionId));
		String delrequestJson = objectMapper.writeValueAsString(delRequest);

		mockMvc.perform(
				delete("/config/v1/formsection").contentType(MediaType.APPLICATION_JSON).content(delrequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.message", is("1"))).andReturn();

	}

// COMPONENT		
	@Test
	void formComponentTestCases() throws Exception {

		FormSectionRequestModel model = new FormSectionRequestModel();
		model.setFormBuilderId(10);
		model.setSectionName("New Test Section");
		model.setSectionOrder(1);
		model.setSectionHelpText("Help Text for Test");
		model.setSectionHeader("Test Header");
		model.setSectionFooter("Test Footer");
		model.setSectionBusinessRule(null);
		model.setSectionDescription("Sample Description");
		model.setIsActive("Y");

		String requestJson = objectMapper.writeValueAsString(model);

		MvcResult result = mockMvc
				.perform(post("/config/v1/formsection").contentType(MediaType.APPLICATION_JSON).content(requestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.description", is("Sample Description")))
				.andExpect(jsonPath("$.helpText", is("Help Text for Test"))).andReturn();

		String jsonResponse = result.getResponse().getContentAsString();
		JsonNode jsonNode = objectMapper.readTree(jsonResponse);
		String formSectionId = jsonNode.path("formBuilderSectionId").asText();

		// CREATE
		FormComponentRequestModel cmodel = new FormComponentRequestModel();
		cmodel.setSectionId(Integer.parseInt(formSectionId));
		cmodel.setFormBuilderId(10);
		cmodel.setComponentType("RT");
		cmodel.setComponentOrder(1);
		cmodel.setComponentData(null);
		cmodel.setComponentRefId(null);
		cmodel.setComponentHeader("Test Header");
		cmodel.setComponentFooter("Test Footer");
		cmodel.setDescription("Sample Description");
		cmodel.setIsActive("Y");

		String crequestJson = objectMapper.writeValueAsString(cmodel);

		MvcResult cresult = mockMvc
				.perform(post("/config/v1/sectioncomponent").contentType(MediaType.APPLICATION_JSON)
						.content(crequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.description", is("Sample Description")))
				.andExpect(jsonPath("$.headerInstruction", is("Test Header"))).andReturn();

		String cjsonResponse = cresult.getResponse().getContentAsString();
		JsonNode cjsonNode = objectMapper.readTree(cjsonResponse);
		String componentId = cjsonNode.path("formBuilderSectCompId").asText();

		// UPDATE -- COMPONENT
		FormComponentRequestModel updatemodel = new FormComponentRequestModel();
		updatemodel.setComponentId(Integer.parseInt(componentId));
		updatemodel.setSectionId(Integer.parseInt(formSectionId));
		updatemodel.setFormBuilderId(10);
		updatemodel.setComponentType("RT");
		updatemodel.setComponentOrder(1);
		updatemodel.setComponentData(null);
		updatemodel.setComponentRefId(null);
		updatemodel.setComponentHeader("Udated Header");
		updatemodel.setComponentFooter("Test Footer");
		updatemodel.setDescription("Sample Description");
		updatemodel.setIsActive("Y");

		String updRequestJson = objectMapper.writeValueAsString(updatemodel);

		mockMvc.perform(
				put("/config/v1/sectioncomponent").contentType(MediaType.APPLICATION_JSON).content(updRequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.description", is("Sample Description")))
				.andExpect(jsonPath("$.headerInstruction", is("Udated Header"))).andReturn();

		// GET - COMPONENT

		mockMvc.perform(get("/config/v1/sectioncomponent/{formBuilderSectCompId}", Integer.parseInt(componentId)))
				.andExpect(status().isOk()).andExpect(jsonPath("$.description", is("Sample Description"))).andReturn();

		// PATCH - COMPONENT

		FormComponentRequestModel patchRequest = new FormComponentRequestModel();
		patchRequest.setComponentId(Integer.parseInt(componentId));
		patchRequest.setSectionId(Integer.parseInt(formSectionId));
		patchRequest.setComponentOrder(1);
		List<FormComponentRequestModel> patchRequestls = List.of(patchRequest);

		String patchRequestJson = objectMapper.writeValueAsString(patchRequestls);

		mockMvc.perform(patch("/config/v1/sectioncomponent/order").contentType(MediaType.APPLICATION_JSON)
				.content(patchRequestJson)).andExpect(status().isOk()).andExpect(jsonPath("$.message", is("1")))
				.andReturn();

		// DELETE

		FormComponentRequestModel delRequest = new FormComponentRequestModel();
		delRequest.setComponentId(Integer.parseInt(componentId));

		String delrequestJson = objectMapper.writeValueAsString(delRequest);

		mockMvc.perform(
				delete("/config/v1/sectioncomponent").contentType(MediaType.APPLICATION_JSON).content(delrequestJson))
				.andExpect(status().isOk()).andExpect(jsonPath("$.message", is("1"))).andReturn();

	}
}
