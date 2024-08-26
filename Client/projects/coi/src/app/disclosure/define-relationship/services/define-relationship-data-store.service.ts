import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { ProjectSfiRelations, CoiDisclEntProjDetail, DefineRelationshipDataStore } from '../../coi-interface';
import { deepCloneObject } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { isExistSearchWord } from '../../../common/utilities/custom-utilities';
import { DefineRelationshipService } from './define-relationship.service';
import { CommonService } from '../../../common/services/common.service';

@Injectable()
export class DefineRelationshipDataStoreService {

    private projectSfiRelationsList: ProjectSfiRelations[] = [];
    private filteredProjectSfiRelationsList: ProjectSfiRelations[] = [];
    $relationsChanged = new Subject<DefineRelationshipDataStore>();

    constructor(private _defineRelationshipService: DefineRelationshipService, private _commonService: CommonService) { }

    private structuredClone<T>(obj: T): T {
        const NATIVE_CLONE_FUNCTION = (window as any).structuredClone;
        return (typeof NATIVE_CLONE_FUNCTION === 'function') ? NATIVE_CLONE_FUNCTION(obj) : JSON.parse(JSON.stringify(obj));
    }

    private getFilteredDisclosureListForSearchWord(payload: DefineRelationshipDataStore): void {
        this.processProjectsSFIDetails(this.projectSfiRelationsList);
        const FILTERED_LIST: ProjectSfiRelations[] = this.projectSfiRelationsList?.filter((projectSfiRelations: ProjectSfiRelations) => {
            const { searchText, searchKeys } = this._defineRelationshipService;
            return isExistSearchWord(projectSfiRelations, searchText, searchKeys, false);
        });
        this.filteredProjectSfiRelationsList = FILTERED_LIST ? deepCloneObject(FILTERED_LIST) : [];
        this._defineRelationshipService.isLoading = false;
        this.$relationsChanged.next(payload);
    }

    private getProjectData(projectList: ProjectSfiRelations[], projectId?: string, keys?: Array<keyof ProjectSfiRelations>): any {
        if (!projectId) {
            // If no projectId is provided, return the entire list
            return this.structuredClone(projectList);
        }

        // Find the specific project by projectId
        const project = projectList.find(p => p.projectId === projectId);
        if (!project) {
            return undefined;
        }

        // If no keys are specified, return the entire project
        if (!keys) {
            return this.structuredClone(project);
        }

        // Retrieve only the specified keys
        const data: any = {};
        keys.forEach(key => {
            if (key in project) {
                data[key] = this.structuredClone(project[key]);
            }
        });
        return data;
    }

    setStoreData(data: ProjectSfiRelations[]): void {
        this.projectSfiRelationsList = this.structuredClone(data);
        const PAY_LOAD: DefineRelationshipDataStore = {
            projectId: 'ALL',
            entityId: 'ALL',
            searchChanged: true,
            updatedKeys: [] // No specific keys updated for full data reset
        };
        this.getFilteredDisclosureListForSearchWord(PAY_LOAD);
    }

    private processProjectsSFIDetails(ProjectSfiRelationsList: ProjectSfiRelations[]): void {
        ProjectSfiRelationsList.forEach((project: ProjectSfiRelations) => {
            const RELATION_TYPE_MAP: { [key: string]: any } = {};
            const ICON_MAP: { [key: string]: any } = {};
    
            project.coiDisclEntProjDetails?.forEach((coiDisclEntProjDetail: CoiDisclEntProjDetail) => {
                coiDisclEntProjDetail.projectConflictStatusCode = coiDisclEntProjDetail.projectConflictStatusCode || null;
                const PERSON_ENTITY = coiDisclEntProjDetail?.personEntity;
                const ENTITY_RELATION_TYPE = PERSON_ENTITY?.validPersonEntityRelType;
    
                if (!PERSON_ENTITY || !ENTITY_RELATION_TYPE) {
                    return;
                }
    
                // Fetch or create the personEntityRelations based on ENTITY_RELATION_TYPE
                if (!RELATION_TYPE_MAP[ENTITY_RELATION_TYPE]) {
                    RELATION_TYPE_MAP[ENTITY_RELATION_TYPE] = this._commonService.getEntityRelationTypePills(ENTITY_RELATION_TYPE);
                }
    
                PERSON_ENTITY.personEntityRelations = RELATION_TYPE_MAP[ENTITY_RELATION_TYPE];
    
                PERSON_ENTITY.personEntityRelations.forEach((entityRelation: any) => {
                    const RELATIONSHIP_TYPE = entityRelation.relationshipType;
    
                    // Fetch or create the icon for the RELATIONSHIP_TYPE
                    if (!ICON_MAP[RELATIONSHIP_TYPE]) {
                        ICON_MAP[RELATIONSHIP_TYPE] = this._commonService.getRelationshipIcon(RELATIONSHIP_TYPE);
                    }
    
                    entityRelation.icon = ICON_MAP[RELATIONSHIP_TYPE];
                });
            });
        });
    }    

    getActualStoreData(projectId?: string, keys?: Array<keyof ProjectSfiRelations>): any {
        return this.getProjectData(this.projectSfiRelationsList, projectId, keys);
    }

    getFilteredStoreData(projectId?: string, keys?: Array<keyof ProjectSfiRelations>): any {
        return this.getProjectData(this.filteredProjectSfiRelationsList, projectId, keys);
    }

    searchTextChanged(): void {
        const PAY_LOAD: DefineRelationshipDataStore = {
            searchChanged: true,
            projectId: 'ALL',
            entityId: 'ALL',
            updatedKeys: [] // No specific keys updated for full data reset
        };
        this.getFilteredDisclosureListForSearchWord(PAY_LOAD);
    }

    // Update or replace a ProjectSfiRelations object in the list
    updateOrReplaceProject(update: ProjectSfiRelations | Partial<ProjectSfiRelations>, keysToUpdate?: Array<keyof ProjectSfiRelations>): void {
        const IS_FULL_UPDATE = 'projectId' in update && Object.keys(update).length === Object.keys(new ProjectSfiRelations()).length;
        let PAY_LOAD = new DefineRelationshipDataStore();
        this.projectSfiRelationsList = this.projectSfiRelationsList?.map(projectSfiRelations => {
            if (projectSfiRelations.projectId === update.projectId) {
                if (keysToUpdate) {
                    // Partial update: only update specified keys
                    const UPDATED_KEYS: string[] = [];
                    const UPDATED_ITEMS: any = { ...projectSfiRelations };
                    keysToUpdate.forEach(key => {
                        if (key in update) {
                            UPDATED_ITEMS[key] = this.structuredClone(update[key]);
                            UPDATED_KEYS.push(key as string);
                        }
                    });
                    // Emit details about the partial update
                    PAY_LOAD = {
                        searchChanged: false,
                        updatedKeys: UPDATED_KEYS,
                        projectId: update.projectId,
                        entityId: UPDATED_KEYS.includes('coiDisclEntProjDetails') ? 'ALL' : null
                    };
                    return UPDATED_ITEMS;
                } else {
                    // Full update: replace the entire projectSfiRelations
                    const UPDATED_ITEMS = IS_FULL_UPDATE
                        ? this.structuredClone(update as ProjectSfiRelations)
                        : this.structuredClone({ ...projectSfiRelations, ...update });

                    // Emit details about the full update
                    const ALL_KEYS = Object.keys(UPDATED_ITEMS) as Array<keyof ProjectSfiRelations>;
                    PAY_LOAD = {
                        searchChanged: false,
                        updatedKeys: ALL_KEYS,
                        projectId: update.projectId,
                        entityId: ALL_KEYS.includes('coiDisclEntProjDetails') ? 'ALL' : null
                    };
                    return UPDATED_ITEMS;
                }
            }
            return projectSfiRelations;
        });
        this.getFilteredDisclosureListForSearchWord(PAY_LOAD);
    }

    // Update or replace coiDisclEntProjDetails within a ProjectSfiRelations object
    updateCoiDisclEntProjDetails(projectId: string, update: Partial<CoiDisclEntProjDetail> | CoiDisclEntProjDetail, keysToUpdate?: Array<keyof CoiDisclEntProjDetail>): void {
        const UPDATED_KEYS: string[] = [];
        const ENTITY_ID: number = update.entityId;

        this.projectSfiRelationsList = this.projectSfiRelationsList?.map(project => {
            if (project.projectId === projectId) {
                const updatedDetails = project.coiDisclEntProjDetails?.map(detail => {
                    if (update instanceof Array) {
                        // If update is an array, handle it by replacing matching items
                        return update.find(u => u.entityId === detail.entityId) || detail;
                    } else if ('entityId' in update && update.entityId === detail.entityId) {
                        if (keysToUpdate) {
                            // Partial update of a specific detail
                            const updatedDetail: CoiDisclEntProjDetail = { ...detail };
                            keysToUpdate.forEach((key: any) => {
                                if (key in update) {
                                    updatedDetail[key] = this.structuredClone(update[key]);
                                    UPDATED_KEYS.push(key as string);
                                }
                            });
                            return updatedDetail;
                        } else {
                            // Full update of a specific detail
                            const updatedDetail = this.structuredClone({ ...detail, ...update });
                            UPDATED_KEYS.push(...Object.keys(updatedDetail));
                            return updatedDetail;
                        }
                    }
                    return detail;
                });
                return { ...project, coiDisclEntProjDetails: updatedDetails };
            }
            return project;
        });
        // Notify listeners about the changes
        const PAY_LOAD: DefineRelationshipDataStore = {
            entityId: ENTITY_ID,
            searchChanged: false,
            projectId: projectId,
            updatedKeys: UPDATED_KEYS
        };
        this.getFilteredDisclosureListForSearchWord(PAY_LOAD);
    }

}
