import { Component, OnInit, Output, EventEmitter, OnDestroy, HostListener } from '@angular/core';
import { CdkDragDrop, moveItemInArray, transferArrayItem } from '@angular/cdk/drag-drop';
import { Form, FormSection, SectionComponent } from '../../shared/form-builder-view/form-builder-interface';
import { FormBuilderCreateService } from '../../form-builder-create.service';
import { ActivatedRoute } from '@angular/router';
import { Subject, Subscription } from 'rxjs';
import { deepCloneObject, scrollIntoView } from '../../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { NewSection, component, ElementTree, CreateComponentObject, FormSectionObject } from '../../form-builder-create-interface';
import { HTTP_ERROR_STATUS } from '../../../../app-constants';
import { CommonService } from '../../../../common/services/common.service';

declare const $: any;
@Component({
    selector: 'app-form-editor',
    templateUrl: './form-editor.component.html',
    styleUrls: ['./form-editor.component.scss']
})
export class FormEditorComponent implements OnInit, OnDestroy {
    scrollTimeOut: any;
    scrollToTop: boolean;
    @HostListener('window:scroll', ['$event'])
    onWindowScroll(event): void {
        if (event) {
            document.querySelector('.floating-btn').classList.add('fb-opacity');
        }
        if (this.scrollTimeOut) {
            clearTimeout(this.scrollTimeOut);
        }

        this.scrollTimeOut = setTimeout(() => {
            this.onScrollEnd();
        }, 100);
    }
    @Output() additionalInformation: EventEmitter<any> = new EventEmitter();
    lookUpTree: Array<ElementTree> = [];
    form = new Form();
    formSection = new FormSection();
    sectionComponent = new SectionComponent();
    formBuilderId: string;
    sectionArray: Array<FormSection> = [];
    additionInfoComponentEvent: Subject<any> = new Subject<any>();
    additionalInfoSectionEvent: Subject<any> = new Subject<any>();
    formSectionOrderNo = 1;
    sectionSortArray: Array<FormSection>;
    lookupSectionComponentType: Array<ElementTree> = [];
    currentlyActiveComponentId: number;
    deleteIndex: number;
    deleteObject: FormSection;
    sectionDelete = false;
    $subscriptions: Subscription[] = [];

    constructor(
        private _formBuilderService: FormBuilderCreateService,
        private _route: ActivatedRoute,
        private _commonService: CommonService) { }

    ngOnInit() {
        this._formBuilderService.currentTab = '1';
        this._route.queryParamMap.subscribe(queryParams => {
            this.formBuilderId = queryParams.get('formBuilderId');
            if (this.formBuilderId) {
                this.initialFormLoad(this.formBuilderId);
            }
        });
    }
    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    onScrollEnd() {
        const floatingButton = document.querySelector('.floating-btn');
        const backToTop = document.querySelector('.fb-form-editor-actions-btn');
        if (window.scrollY > 250) {
            this.scrollToTop = true;
            backToTop.classList.add('floating-Btn-width');
        } else {
            this.scrollToTop = false;
            backToTop.classList.remove('floating-Btn-width');
        }
        floatingButton.classList.remove('fb-opacity');
    }

    drop(event: CdkDragDrop<string[]>): void {
        if (event.previousContainer === event.container) {
            moveItemInArray(event.container.data, event.previousIndex, event.currentIndex);
        } else {
            transferArrayItem(event.previousContainer.data,
                event.container.data,
                event.previousIndex,
                event.currentIndex);
        }
        this.setComponent(event);
        this.lookUpTree = deepCloneObject(this.lookupSectionComponentType);
        this.setFocusOnActiveComponent();
    }

    setFocusOnActiveComponent() {
        setTimeout(() => {
            if (this.currentlyActiveComponentId) {
                this.componentBorder(this.currentlyActiveComponentId);
            }
        }, 1000);
    }

    // Emit component related data to Additional information Component
    emitComponentData(item): void {
        this.currentlyActiveComponentId = item.componentId || item.tempId;
        this.componentBorder(this.currentlyActiveComponentId);
        this.additionInfoComponentEvent.next(item);
    }

    createNewSection(): void {
        this.$subscriptions.push(
            this._formBuilderService.createFormSection(this.createNewSectionObject()).subscribe((data: NewSection) => {
                this.formSection.sectionId = data.sectionId;
                this.formSection.sectionName = data.sectionName;
                this.formSection.sectionDescription = data.sectionDescription;
                this.formSection.sectionHeader = data.sectionHeader;
                this.formSection.sectionFooter = data.sectionFooter;
                this.formSection.sectionHelpText = data.sectionHelpText;
                this.formSection.sectionBusinessRule = data.sectionBusinessRule;
                this.formSection.sectionOrder = data.sectionOrder;
                this.formSection.sectionComponent = [];
                const formSection = deepCloneObject(this.formSection);
                this.sectionArray.push(formSection);
                this._formBuilderService.formEditorState = deepCloneObject(this.sectionArray);
                this.scrollToNewSection();
            })
        );
    }

    scrollToNewSection() {
        setTimeout(() => {
            scrollIntoView(String(this.formSection.sectionId));
        }, 1000);
    }

    initialFormLoad(formBuilderId: string): void {
        this.$subscriptions.push(
            this._formBuilderService.getFormDeatails(formBuilderId).subscribe((data: any) => {
                this.lookupSectionComponentType = data.lookupSectionComponentType;
                this.lookUpTree = deepCloneObject(this.lookupSectionComponentType);
                this.sectionArray = data.formHeader.sections;
                this._formBuilderService.formEditorState = deepCloneObject(this.sectionArray);
                if (!this.sectionArray.length) {
                    this.createNewSection();
                    setTimeout(() => {
                        document.getElementById('first-Section').click();
                    }, 1000);
                }
            })
        );
    }

    createNewSectionObject(): FormSectionObject {
        if (this.sectionArray.length) {
            this.formSectionOrderNo = this.sectionArray[this.sectionArray.length - 1].sectionOrder + 1;
        }
        const formSectionObject = new FormSectionObject();
        formSectionObject.formBuilderId = this.formBuilderId;
        formSectionObject.sectionName = '';
        formSectionObject.sectionOrder = this.formSectionOrderNo;
        formSectionObject.sectionBusinessRule = null;
        formSectionObject.sectionDescription = '';
        formSectionObject.sectionHelpText = 'Help Text 1';
        formSectionObject.sectionHeader = '';
        formSectionObject.sectionFooter = '';
        formSectionObject.isActive = 'Y';
        return formSectionObject;
    }

    // This function sets requied values for adding a component from the event of drop function provide by the Angular Drag & Drop cdk.
    setComponent(event): void {
        const formSectionId = event.container.id;
        // container is an array that  receives the dragged component
        const container = event.container.data;
        // index of array at which component was droped.
        const containerindex = event.currentIndex;

        const previousContainer = event.previousContainer;
        // dropContainer is the Section that  receives the dragged component
        const dropContainer = this.sectionArray.find(obj => obj['sectionId'] == formSectionId);
        this.createcomponent(formSectionId, containerindex, container, dropContainer, previousContainer);

    }

    createcomponent(formSectionId: string, containerindex: number, container, dropContainer, previousContainer): void {
        if (previousContainer.id.includes('cdk-drop-list-')) {
            //  if condition is satisfied for an element dragged from form-element tree,
            //  and not satisfied for interdragged components ie, components dragged b/w sections or  with in the section.
            if (['BR', 'HL'].includes(container[containerindex].componentTypeCode)) {
                this.onDropSaveForNonConfigurableComponents(formSectionId, containerindex, container, dropContainer);
                return;
            }
            let sectionComponent;
            const uniqueID = 'tempId_' + this.getTimeStamp();
            sectionComponent = dropContainer.sectionComponent[containerindex];
            sectionComponent.tempId = uniqueID;
            sectionComponent.sectionId = dropContainer.sectionId;
            this.getTempOrderNumberForComponents(dropContainer.sectionComponent);
            sectionComponent.formBuilderId = this.formBuilderId;
            this._formBuilderService.formEditorState = deepCloneObject(this.sectionArray);
        } else {
            let sectionComponent;
            sectionComponent = dropContainer.sectionComponent[containerindex];
            sectionComponent.sectionId = dropContainer.sectionId;
            this.getTempOrderNumberForComponents(dropContainer.sectionComponent);
            this._formBuilderService.formEditorState = deepCloneObject(this.sectionArray);
            this.updatePositionOfAllComponentsInSection(container, dropContainer);

        }
    }

    onDropSaveForNonConfigurableComponents(formSectionId: string, containerindex: number, container, dropContainer): void {
        this.$subscriptions.push(
            this._formBuilderService.createComponent(this.prepareComponentObject(formSectionId, containerindex, container))
                .subscribe((data: component) => {
                    this.sectionComponent.componentDescription = data.componentDescription;
                    this.sectionComponent.componentId = data.componentId;
                    this.sectionComponent.componentOrder = data.componentOrder;
                    this.sectionComponent.componentType = data.componentType;
                    this.sectionComponent.sectionId = data.sectionId;
                    this.sectionComponent.label = data.label;
                    this.sectionComponent.componentTypeDescription = data.componentTypeDescription;
                    const sectionComponent = deepCloneObject(this.sectionComponent);
                    dropContainer.sectionComponent[containerindex] = sectionComponent;
                    this.updatePositionOfAllComponentsInSection(container, dropContainer);
                    this._formBuilderService.formEditorState = deepCloneObject(this.sectionArray);
                })
        );
    }

    prepareComponentObject(formSectionId: string, containerindex: number, container, componentRefId = ''): CreateComponentObject {
        let label = '';
        if (container[containerindex].componentTypeCode === 'BR') {
            label = 'Context Break';
        } else {
            label = 'Horizontal Line';
        }
        const createComponentObject = new CreateComponentObject();
        createComponentObject.sectionId = formSectionId;
        createComponentObject.formBuilderId = this.formBuilderId;
        createComponentObject.componentType = container[containerindex].componentTypeCode;
        createComponentObject.componentOrder = containerindex;
        createComponentObject.componentData = '';
        createComponentObject.componentRefId = componentRefId;
        createComponentObject.description = 'test';
        createComponentObject.componentFooter = '';
        createComponentObject.componentHeader = '';
        createComponentObject.isActive = 'Y';
        createComponentObject.componentTypeDescription = container[containerindex].description;
        createComponentObject.label = label;
        return createComponentObject;
    }

    updatePositionOfAllComponentsInSection(container, dropContainer): void {
        const componets = container.filter(ele => ele.componentId);
        componets.forEach((element, index) => {
            this.$subscriptions.push(
                this._formBuilderService.componentOrder([{
                    'componentId': element.componentId,
                    'sectionId': dropContainer.sectionId,
                    'componentOrder': index
                }]).subscribe((data) => { })
            );
        });
    }

    confirmComponentDelete(index: number, deleteObject: FormSection, event): void {
        event.stopPropagation();
        this.sectionDelete = false;
        $('#delete-Confirmation-Modal').modal('show');
        this.deleteIndex = index;
        this.deleteObject = deleteObject;
    }

    deleteComponent(): void {
        const indexTobeDeleted = this.deleteObject.sectionComponent[this.deleteIndex].componentId;
        if (!indexTobeDeleted) {
            this.deleteObject.sectionComponent.splice(this.deleteIndex, 1);
            this._formBuilderService.formEditorState = deepCloneObject(this.sectionArray);
            this.additionInfoComponentEvent.next({});
        } else {
            this.$subscriptions.push(
                this._formBuilderService.deleteComponent(indexTobeDeleted).subscribe((data) => {
                    this.deleteObject.sectionComponent.splice(this.deleteIndex, 1);
                    this._formBuilderService.formEditorState = deepCloneObject(this.sectionArray);
                    this.additionInfoComponentEvent.next({});
                })
            );
        }
    }

    sectionSort(): void {
        if (this.sectionArray?.length <= 1) {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Need atleast two sections to rearrange.');
            return;
        }
        $('#rearrange-section-modal').modal('show');
        setTimeout(() => {
            const backDrop = document.querySelector('.modal-backdrop');
            backDrop.classList.remove('modal-backdrop');
            backDrop.classList.add('fb-modal-backdrop');

        }, 50);
        this.sectionSortArray = deepCloneObject(this.sectionArray);
    }

    dropForSort(event: CdkDragDrop<string[]>) {
        moveItemInArray(this.sectionSortArray, event.previousIndex, event.currentIndex);
    }

    sectionSortService(): void {
        this.sectionSortArray.forEach((element, index) => {
            this.$subscriptions.push(
                this._formBuilderService.sectionOrder([{
                    'sectionId': element.sectionId,
                    'formBuilderId': this.formBuilderId,
                    'sectionOrder': index + 1,
                }]).subscribe((data: Array<FormSection>) => {
                    this.sectionArray = this.sectionSortArray;
                })
            );
        });
    }

    cancelSectionSort(): void {
        this.sectionSortArray = [];
    }


    emitSectionData(event): void {
        this.sectionBorder(event.sectionId);
        this.additionalInfoSectionEvent.next(event);
    }

    sectionUpdate(event): void {
        let selectedSection: any;
        selectedSection = this.sectionArray.filter(ele => ele.sectionId === event.sectionId);
        selectedSection[0].sectionName = event.sectionName;
    }

    componentUpdate(event): void {
        let selectedComponent: any;
        for (const element of this.sectionArray) {
            selectedComponent = element.sectionComponent.find(ele => ele.componentId === event.componentData.componentId);
            if (selectedComponent) {
                selectedComponent.label = event.componentData.label;
                selectedComponent.isMandatory = event.componentData.isMandatory;
                selectedComponent.validationMessage = event.componentData.validationMessage;
                selectedComponent.componentData = event.componentData.componentData;
                return;
            }
        }
    }

    deleteSectionConfirmation(deleteObject: FormSection, sectionArrayIndex: number): void {
        this.sectionDelete = true;
        $('#delete-Confirmation-Modal').modal('show');
        this.deleteObject = deleteObject;
        this.deleteIndex = sectionArrayIndex;
    }

    deleteSection(): void {
        this.sectionBorder(this.deleteObject.sectionId);
        this.$subscriptions.push(
            this._formBuilderService.deleteSection(this.deleteObject.sectionId).subscribe((data) => {
                this.sectionArray.splice(this.deleteIndex, 1);
                this._formBuilderService.formEditorState = deepCloneObject(this.sectionArray);
                this.additionalInfoSectionEvent.next({});
            })
        );
    }

    sectionBorder(selectionId: number): void {
        if (document.getElementsByClassName('border-for-section-selection')[0]) {
            document.getElementsByClassName('border-for-section-selection')[0].classList.remove('border-for-section-selection');
        }
        if (document.getElementsByClassName('clicked')[0]) {
            document.getElementsByClassName('clicked')[0].classList.remove('clicked');
        }
        const backgroundColor = document.getElementById(`section-card-${selectionId}`).classList.add('border-for-section-selection');
    }

    componentBorder(componentId: number): void {
        if (document.getElementsByClassName('clicked')[0]) {
            document.getElementsByClassName('clicked')[0]?.classList.remove('clicked');
        }
        if (document.getElementsByClassName('border-for-section-selection')[0]) {
            document.getElementsByClassName('border-for-section-selection')[0]?.classList.remove('border-for-section-selection');
        }
        const backgroundColor = document.getElementById(`field-box-${componentId}`).classList.add('clicked');
    }

    getTimeStamp(): number {
        return new Date().getTime();
    }

    getTempOrderNumberForComponents(sectionComponent): void {
        sectionComponent.forEach((component, index) => {
            component.componentOrderNumber = index;
        });
    }

    initialComponentSave(event): void {
        let selectedComponent;
        for (let ele of this.sectionArray) {
            selectedComponent = ele.sectionComponent.find(ele => ele.tempId === this._formBuilderService.currentComponentPosition.tempId);
            if (selectedComponent) {
                delete selectedComponent.tempId;
                this.currentlyActiveComponentId = event.componentId;
                selectedComponent.componentId = event.componentId;
                selectedComponent.label = event.label;
                const selectedSection = this.sectionArray.find(ele => ele.sectionId === event.sectionId);
                this.updatePositionOfAllComponentsInSection(selectedSection.sectionComponent, selectedSection);
                this._formBuilderService.formEditorState = deepCloneObject(this.sectionArray);
                return;
            }
        }
    }

    scrollUp():void {
        scrollIntoView(String(this.sectionArray[0].sectionId));
    }

    closeBtn(id: string) {
        $(id).modal('hide');

    }
}
