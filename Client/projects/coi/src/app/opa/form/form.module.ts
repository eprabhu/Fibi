import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormComponent} from './form.component';
import {RouterModule, Routes} from '@angular/router';
import {FormBuilderViewComponent} from '../../shared/form-builder-view/form-builder-view.component';
import {FormSectionsComponent} from '../../shared/form-builder-view/form-sections/form-sections.component';
import {SharedModule} from '../../shared/shared.module';

const routes: Routes = [{path: '', component: FormComponent}];

@NgModule({
    declarations: [
        FormComponent,
        FormBuilderViewComponent,
        FormSectionsComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        SharedModule
    ]
})
export class FormModule {
}
