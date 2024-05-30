import { Injectable } from '@angular/core';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Injectable()
export class InformationAndHelpTextService {

    moduleConfiguration = {};


    constructor() { }

    getInFormationText(subSectionId: any, elementId: any) {
        if (!isEmptyObject(this.moduleConfiguration)) {
            if (subSectionId && elementId) {
                let elementDetails = this.moduleConfiguration[subSectionId].elementConfig.find(ele => ele.uiReferenceId === elementId);
                if (elementDetails) {
                    return elementDetails.instruction;
                }
            } else if (subSectionId) {
                return this.moduleConfiguration[subSectionId].instruction;
            }
        }
    }

    getHelpText(subSectionId: any, elementId: any) {
        if (!isEmptyObject(this.moduleConfiguration)) {
            if (subSectionId && elementId) {
                let elementDetails = this.moduleConfiguration[subSectionId].elementConfig.find(ele => ele.uiReferenceId === elementId);
                if (elementDetails) {
                    return elementDetails.help;
                }
            } else if (subSectionId) {
                return this.moduleConfiguration[subSectionId].help;
            }
        }
    }

}
