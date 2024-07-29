package com.polus.fibicomp.coi.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dto.ProjectCommentDto;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;

@Transactional
@Service
public interface ProjectService {

	/**
	 * This method is used to save comments against project
	 * 
	 * @param comment
	 */
	ResponseEntity<Object> saveComment(ProjectCommentDto dto);

	/**
	 * This method is used to update comments against project
	 * 
	 * @param dto
	 */
	ResponseEntity<Object> updateComment(ProjectCommentDto dto);

	/**
	 * This method is used to fetch comments against project
	 * 
	 * @param dto
	 * @return List of CoiProjectComment
	 */
	ResponseEntity<Object> fetchComment(ProjectCommentDto dto);

	/**
	 * This method is used to delete comments against project
	 * 
	 * @param commentId
	 */
	ResponseEntity<Object> deleteComment(Integer commentId);

	/**
	 * This method is used to fetch project overview
	 * 
	 * @param vo
	 * @return List of disclosure project dtos
	 */
	ResponseEntity<Object> fetchDashbaord(CoiDashboardVO vo);

	/**
	 * This method is used to fetch project status lookup
	 * @param projectType 
	 * 
	 * @return List of project status
	 */
	ResponseEntity<Object> getProjectStatusLookup(String projectType);

}
