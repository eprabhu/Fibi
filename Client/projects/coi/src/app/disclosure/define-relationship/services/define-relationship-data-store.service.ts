import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { ProjectSfiRelations, CoiDisclEntProjDetail, DefineRelationshipDataStore } from '../../coi-interface';

@Injectable()
export class DefineRelationshipDataStoreService {

    private projectSfiRelationsList: ProjectSfiRelations[] = [];
    relationsChanged = new Subject<DefineRelationshipDataStore>();
    isEditMode = false;

    constructor() { }

    private structuredClone<T>(obj: T): T {
        const NATIVE_CLONE_FUNCTION = (window as any).structuredClone;
        return (typeof NATIVE_CLONE_FUNCTION === 'function') ? NATIVE_CLONE_FUNCTION(obj) : JSON.parse(JSON.stringify(obj));
    }

    setStoreData(data: ProjectSfiRelations[]): void {
        this.projectSfiRelationsList = this.structuredClone(data);
        this.relationsChanged.next({
            projectId: 'ALL',
            entityId: 'ALL',
            updatedKeys: [] // No specific keys updated for full data reset
        });
    }

    // Retrieve the entire list of ProjectSfiRelations or specific keys
    getStoreData(projectId?: string, keys?: Array<keyof ProjectSfiRelations>): any {
        if (!projectId) {
            // If no projectId is provided, return the entire list
            return this.structuredClone(this.projectSfiRelationsList);
        }
        
        // Find the specific project by projectId
        const project = this.projectSfiRelationsList.find(p => p.projectId === projectId);
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

    // Update or replace a ProjectSfiRelations object in the list
    updateOrReplaceProject(update: ProjectSfiRelations | Partial<ProjectSfiRelations>, keysToUpdate?: Array<keyof ProjectSfiRelations>): void {
        const IS_FULL_UPDATE = 'projectId' in update && Object.keys(update).length === Object.keys(new ProjectSfiRelations()).length;
        let PAY_LOAD = new DefineRelationshipDataStore();
        this.projectSfiRelationsList = this.projectSfiRelationsList.map(projectSfiRelations => {
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
                        projectId: update.projectId,
                        entityId: UPDATED_KEYS.includes('coiDisclEntProjDetails') ? 'ALL' : null,
                        updatedKeys: UPDATED_KEYS
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
                        projectId: update.projectId,
                        entityId: ALL_KEYS.includes('coiDisclEntProjDetails') ? 'ALL' : null,
                        updatedKeys: ALL_KEYS,
                    };
                    return UPDATED_ITEMS;
                }
            }
            return projectSfiRelations;
        });
        this.relationsChanged.next(PAY_LOAD);
    }

    // Update or replace coiDisclEntProjDetails within a ProjectSfiRelations object
    updateCoiDisclEntProjDetails(projectId: string, update: Partial<CoiDisclEntProjDetail> | CoiDisclEntProjDetail, keysToUpdate?: Array<keyof CoiDisclEntProjDetail>): void {
        const UPDATED_KEYS: string[] = [];
        const ENTITY_ID: number = update.entityId;

        this.projectSfiRelationsList = this.projectSfiRelationsList.map(project => {
            if (project.projectId === projectId) {
                const updatedDetails = project.coiDisclEntProjDetails.map(detail => {
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
        this.relationsChanged.next({
            projectId,
            entityId: ENTITY_ID,
            updatedKeys: UPDATED_KEYS
        });
    }

}
