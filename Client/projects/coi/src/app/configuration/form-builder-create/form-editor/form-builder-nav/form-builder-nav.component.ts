import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { FormBuilderCreateService } from '../../form-builder-create.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { FormHeaderResponse, LoadForm, UpdateFormHeaderObject } from '../../form-builder-create-interface';
declare const $: any;


@Component({
    selector: 'app-form-builder-nav',
    templateUrl: './form-builder-nav.component.html',
    styleUrls: ['./form-builder-nav.component.scss']
})
export class FormBuilderNavComponent implements OnInit, OnDestroy {
    formBuilderId: string;
    editFormTitle: boolean = false;
    formTitle: string = "";
    $subscriptions: Subscription[] = [];
    Birdview = false;
    formBuilderNumber: string;
    isFormPublishable = false;
    publisModalHeading: string;
    publisModalMsg: string;
    formValidation = new Map();
    tempForFormTitle = "";
    formDescription = "";

    constructor(private _route: ActivatedRoute, public _formBuilderService: FormBuilderCreateService,
        private navigation: Router) { }

    ngOnInit() {
        this._route.queryParamMap.subscribe(queryParams => {
            this.formBuilderId = queryParams.get('formBuilderId');
            this.serviceForLoadingForm(this.formBuilderId);
        })

    }

    serviceForLoadingForm(formBuilderId: string): void {
        this.$subscriptions.push(
            this._formBuilderService.getFormDeatails(formBuilderId).subscribe((data: LoadForm) => {
                this.formDescription = data.formHeader.description;
                this.formTitle = data.formHeader.title;
                this.tempForFormTitle = JSON.parse(JSON.stringify(this.formTitle))
                this.formBuilderNumber = data.formHeader.formBuilderNumber;
            })
        );
    }

    prepareFormHeaderObject(): UpdateFormHeaderObject {
        const versionStatus = (this.isFormPublishable) ? 'Y' : 'N';
        const formHeaderObject = new UpdateFormHeaderObject();
        formHeaderObject.formBuilderId = this.formBuilderId;
        formHeaderObject.title = this.formTitle;
        formHeaderObject.description = this.formDescription;
        formHeaderObject.isActive = versionStatus;
        return formHeaderObject;
    }

    saveTitle(): void {
        this.isTitleEmpty();
        this.$subscriptions.push(
            this._formBuilderService.updateFormHeader(this.prepareFormHeaderObject()).subscribe((data: FormHeaderResponse) => {
                this.editFormTitle = false;
                this.tempForFormTitle = data.title;
            })
        );
    }

    editTitle(): void {
        setTimeout(() => {
            document.getElementById('edit-Input').focus()
        }, 100);
    }

    navigateToTab(tab: string): void {
        switch (tab) {
            case '1':
                this.navigation.navigate(['/coi/form-builder-create/form-editor/editor'], { queryParams: { formBuilderId: this.formBuilderId } });
                break;
            case '2':
                this.navigation.navigate(['/coi/form-builder-create/form-editor/integration'], { queryParams: { formBuilderId: this.formBuilderId, title: this.formTitle, formBuilderNumber: this.formBuilderNumber } });
                break;
            case '3':
                this.navigation.navigate(['/coi/form-builder-create/form-editor/preview'], { queryParams: { formBuilderId: this.formBuilderId } });
                break;
        }
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    canFormBePublished(): void {
        const isEmptySectionPresent = this._formBuilderService.isEmptySectionPresent();
        const isUnconfiguredcomponentsPresent = this._formBuilderService.isUnconfiguredcomponentsPresent();

        if (this._formBuilderService.formEditorState.length == 0) {
            this.isFormPublishable = false;
            this.publisModalHeading = "Warning";
            this.publisModalMsg = "Empty Form cannot be published.";

        } else if (!isEmptySectionPresent && !isUnconfiguredcomponentsPresent) {
            this.isFormPublishable = true;
            this.publisModalHeading = "Publish Form";
            this.publisModalMsg = "Are you sure you want to publish this form?";


        } else {
            this.isFormPublishable = false;
            this.publisModalHeading = "Warning";
            this.publisModalMsg = 'You cannot publish a form with' + (isUnconfiguredcomponentsPresent ? " unconfigured components or " : " ") + 'Empty sections.';

        }
        $('#publish-Modal').modal('show');
    }

    publishForm(): void {
        this.$subscriptions.push(
            this._formBuilderService.publishForm(this.prepareFormHeaderObject()).subscribe((data: FormHeaderResponse) => {
            })
        )
    }

    isTitleEmpty(): void {
        if (this.formTitle == "") {
            this.formTitle = this.tempForFormTitle;
        }

    }

}
