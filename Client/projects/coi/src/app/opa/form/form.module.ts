import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormComponent} from './form.component';
import {RouterModule, Routes} from '@angular/router';
import {SharedModule} from '../../shared/shared.module';
import { FormSharedModule } from '../../configuration/form-builder-create/shared/shared.module';

const routes: Routes = [{path: '', component: FormComponent}];

@NgModule({
    declarations: [
        FormComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        SharedModule,
        FormSharedModule
    ]
})
export class FormModule {
}
