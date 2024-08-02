package com.polus.fibicomp.coi.controller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.coi.dto.ProjectCommentDto;
import com.polus.fibicomp.coi.service.ProjectService;
import com.polus.fibicomp.coi.vo.CoiDashboardVO;

@RestController
@RequestMapping("/coi/project")
public class ProjectController {

	protected static Logger logger = LogManager.getLogger(ProjectController.class.getName());

	@Autowired
	private ProjectService projectService;

	@PostMapping("/fetchDashbaord")
	public ResponseEntity<Object> fetchDashbaord(@RequestBody CoiDashboardVO vo) {
		return projectService.fetchDashbaord(vo);
	}

	@GetMapping("/getProjectStatusLookup/{projectType}")
	public ResponseEntity<Object> getProjectStatusLookup(@PathVariable(value = "projectType", required = true) final String projectType) {
		return projectService.getProjectStatusLookup(projectType);
	}

	@PostMapping("/saveComment")
	public ResponseEntity<Object> saveComment(@RequestBody ProjectCommentDto dto) {
		return projectService.saveComment(dto);
	}

	@PatchMapping("/updateComment")
	public ResponseEntity<Object> updateComment(@RequestBody ProjectCommentDto dto) {
		return projectService.updateComment(dto);
	}

	@PostMapping("/fetchComment")
	public ResponseEntity<Object> fetchComment(@RequestBody ProjectCommentDto dto) {
		return projectService.fetchComment(dto);
	}

	@DeleteMapping("/deleteComment/{commentId}")
	public ResponseEntity<Object> deleteComment(@PathVariable(value = "commentId", required = true) final Integer commentId) {
		return projectService.deleteComment(commentId);
	}

}
