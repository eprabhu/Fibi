import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { FormBuilderCreateService } from '../../form-builder-create.service'
import { FetchModuleCode, FormIntegration, GetAllFormUsage, ModuleList, IntegrtionResponse, SubModules, UpdateFormUsage, saveFormUsage } from '../../form-builder-create-interface'

declare const $: any;

@Component({
    selector: 'app-form-integration',
    templateUrl: './form-integration.component.html',
    styleUrls: ['./form-integration.component.scss']
})
export class FormIntegrationComponent implements OnInit, OnDestroy {
    formBuilderId: string;
    formIntegtion = new FormIntegration;
    $subscriptions: Subscription[] = [];
    moduleList: ModuleList[];
    subModuleList: SubModules[];
    formBuilderNumber: string;
    formUsage: GetAllFormUsage[];
    formUsageList = [];
    editStatus = "";
    deleteIndex: number;
    editIndex: number;
    formValidation = new Map();

    constructor(private _route: ActivatedRoute, public formBuilderService: FormBuilderCreateService) { }

    ngOnInit() {
        this._route.queryParamMap.subscribe(queryParams => {
            this.formBuilderId = queryParams.get('formBuilderId');
            this.formBuilderNumber = queryParams.get('formBuilderNumber');
        });
        this.fetchModuleCode();
    }

    fetchModuleCode(): void {
        this.formBuilderService.getModuleList().subscribe((data: FetchModuleCode) => {
            this.moduleList = this.mapModules(data.moduleList);
            this.getAllFormUsage();
        });
    }

    mapModules(list: Array<any>): Array<any> {
        const MODULES = list.filter(el => !el.SUB_MODULE_CODE);
        MODULES.forEach(m => m.subModules = this.findSubModules(list, m.MODULE_CODE));
        return MODULES;
    }

    findSubModules(list, code): Array<any> {
        const SUB_MODULES = list.filter(l => l.MODULE_CODE === code && l.SUB_MODULE_CODE);
        return SUB_MODULES;
    }

    findSubModuleList(modulecode = this.formIntegtion.formModuleCode): void {
        this.formIntegtion.formSubSectionCode = null;
        const LIST: any = this.moduleList.find(M => M.MODULE_CODE == modulecode);
        this.subModuleList = LIST && LIST.subModules ? this.sortSubModules(LIST.subModules) : [];
    }

    sortSubModules(subModuleList: any): any[] {
        subModuleList.sort((a, b) => {
            return a.DESCRIPTION.toLowerCase() > b.DESCRIPTION.toLowerCase() ? 1 :
                a.DESCRIPTION.toLowerCase() < b.DESCRIPTION.toLowerCase() ? -1 : 0;
        });
        return subModuleList;
    }

    saveIntegration(): void {
        if (this.isModuleCodeValid()) {
            $('#add-Integration-Modal').modal('hide');
            let moduleName;
            let subSectionName;
            this.$subscriptions.push(
                this.formBuilderService.saveFormUsage(this.prepareFormUsageObject()).subscribe((data: IntegrtionResponse) => {
                    moduleName = this.moduleList.find((x) => x.MODULE_CODE == data.moduleCode)
                    subSectionName = moduleName.subModules.find((subSection) =>
                        subSection.SUB_MODULE_CODE == data.subModuleCode)
                    this.formUsageList.push(
                        {
                            formUsageId: data.formUsageId,
                            formOrderNumber: data.formOrderNumber,
                            moduleName: moduleName.DESCRIPTION,
                            subSectionName: subSectionName?.DESCRIPTION,
                            moduleCode: moduleName?.MODULE_CODE,
                            subSectionCode: subSectionName?.SUB_MODULE_CODE || 0,
                        }
                    )
                    this.resetUsage();
                })
            )
        }

    }

    prepareFormUsageObject(): saveFormUsage {
        const formUsageObject = new saveFormUsage();
        formUsageObject.formBuilderNumber = this.formBuilderNumber;
        formUsageObject.formBuilderId = this.formBuilderId;
        formUsageObject.moduleCode = this.formIntegtion.formModuleCode;
        formUsageObject.subModuleCode = this.formIntegtion.formSubSectionCode || 0,
        formUsageObject.businessRuleId = null;
        formUsageObject.description = "sample";
        formUsageObject.isActive = "Y";
        return formUsageObject;
    }

    getAllFormUsage(): void {
        this.$subscriptions.push(
            this.formBuilderService.getAllFormUsage(this.formBuilderId).subscribe((data: GetAllFormUsage[]) => {
                if (data) {
                    let moduleName;
                    let subSectionName;
                    this.formUsage = data;
                    this.formUsage.forEach((ele) => {
                        moduleName = this.moduleList.find((x) => x.MODULE_CODE == ele.moduleCode)
                        subSectionName = moduleName.subModules.find((subSection) =>
                            subSection.SUB_MODULE_CODE == ele.subModuleCode)
                        this.formUsageList.push(
                            {
                                formUsageId: ele.formUsageId,
                                moduleName: moduleName.DESCRIPTION,
                                subSectionName: subSectionName?.DESCRIPTION,
                                moduleCode: moduleName?.MODULE_CODE,
                                formOrderNumber: ele.formOrderNumber,
                                subSectionCode: subSectionName?.SUB_MODULE_CODE || 0
                            }
                        )
                    })
                }
            })
        )
    }


    editUsage(item, status: string, index: number): void {
        if (status === "E") {
            this.editIndex = index;
            this.editStatus = "E";
            this.formIntegtion.formModuleCode = item.moduleCode;
            this.findSubModuleList();
            this.formIntegtion.formSubSectionCode = item.subSectionCode;
            this.formIntegtion.formUsageId = item.formUsageId;
            this.formIntegtion.formOrderNumber = item.formOrderNumber;
        }
    }

    prepareFormUsageObjectForUpdate(): UpdateFormUsage {
        const formUsageObject = new UpdateFormUsage();
        formUsageObject.formUsageId = this.formIntegtion.formUsageId;
        formUsageObject.formBuilderNumber = this.formBuilderNumber;
        formUsageObject.formBuilderId = this.formBuilderId;
        formUsageObject.moduleCode = this.formIntegtion.formModuleCode;
        formUsageObject.subModuleCode = this.formIntegtion.formSubSectionCode || 0,
        formUsageObject.businessRuleId = null;
        formUsageObject.description = "sample";
        formUsageObject.isActive = "Y";
        formUsageObject.formOrderNumber = this.formIntegtion.formOrderNumber;
        return formUsageObject;
    }

    // Description should be given from back-end.

    updateFormUsage(): void {
        if (this.isModuleCodeValid()) {
            $('#add-Integration-Modal').modal('hide');
            let moduleName;
            let subSectionName;
            this.$subscriptions.push(
                this.formBuilderService.updateFormUsage(this.prepareFormUsageObjectForUpdate()).subscribe((data: IntegrtionResponse) => {
                    this.formUsageList[this.editIndex].moduleCode = Number(data.moduleCode);
                    this.formUsageList[this.editIndex].subSectionCode = Number(data.subModuleCode);
                    this.formUsageList[this.editIndex].formUsageId = data.formUsageId;
                    moduleName = this.moduleList.find((x) => x.MODULE_CODE == data.moduleCode)
                    subSectionName = moduleName.subModules.find((subSection) =>
                        subSection.SUB_MODULE_CODE == data.subModuleCode)
                    this.formUsageList[this.editIndex].moduleName = moduleName.DESCRIPTION;
                    this.formUsageList[this.editIndex].subSectionName = subSectionName?.DESCRIPTION;
                    this.resetUsage();
                })
            )
        }
    }

    resetUsage(): void {
        this.formValidation.clear();
        this.editStatus = "";
        this.formIntegtion = new FormIntegration;
    }

    deleteUsage(): void {
        this.formBuilderService.deleteusage(this.formIntegtion.formUsageId).subscribe((data) => {
            this.formUsageList.splice(this.deleteIndex, 1);
        })
    }

    isModuleCodeValid(): boolean {
        this.formValidation.clear();
        if (!this.formIntegtion.formModuleCode) {
            this.formValidation.set("moduleCodeValidation", true);
        }
        if (this.formValidation.size > 0) {
            return false;
        }
        return true;
    }

    openIntegrationModal(): void {
        $('#add-Integration-Modal').modal('show');

    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}
