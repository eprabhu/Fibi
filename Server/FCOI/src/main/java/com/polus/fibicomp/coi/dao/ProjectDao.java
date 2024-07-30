package com.polus.fibicomp.coi.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dto.DisclosureProjectDto;
import com.polus.fibicomp.coi.dto.ProjectCommentDto;
import com.polus.fibicomp.coi.dto.ProjectStatusLookupDto;
import com.polus.fibicomp.coi.pojo.CoiProjectComment;
import com.polus.fibicomp.coi.vo.CoiDashboardVO;

@Transactional
@Service
public interface ProjectDao {

	/**
	 * This method is used to save comments against project
	 * 
	 * @param comment
	 */
	public void saveComment(CoiProjectComment comment);

	/**
	 * This method is used to update comments against project
	 * 
	 * @param dto
	 */
	public void updateComment(ProjectCommentDto dto);

	/**
	 * This method is used to fetch comments against project
	 * 
	 * @param dto
	 * @return List of CoiProjectComment
	 */
	public List<CoiProjectComment> fetchComment(ProjectCommentDto dto);

	/**
	 * This method is used to delete comments against project
	 * 
	 * @param commentId
	 */
	public void deleteComment(Integer commentId);

	/**
	 * This method is used to check if child comments are present
	 * 
	 * @param commentId
	 * @return True if no child comment is present, False if child comments are present
	 */
	public Boolean canDeleteComment(Integer commentId);

	/**
	 * This method is used to fetch project overview
	 * 
	 * @param vo
	 * @return List of disclosure project dtos
	 */
	List<DisclosureProjectDto> fetchProjectOverview(CoiDashboardVO vo);

	/**
	 * This method is used to fetch project overview count
	 * 
	 * @param vo
	 * @return count of projects
	 */
	public Integer fetchProjectOverviewCount(CoiDashboardVO vo);

	/**
	 * This method is used to fetch project status lookup
	 * 
	 * @return List of project statuses
	 */
	public List<ProjectStatusLookupDto> getProposalStatusLookup();

}
