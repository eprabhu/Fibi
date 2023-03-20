import {NgModule} from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {AppRouterComponent} from "./common/app-router/app-router.component";

const routes: Routes = [
    {path: '', redirectTo: 'coi', pathMatch: 'full'},
    {path: 'coi', component: AppRouterComponent},
    {path: 'login', loadChildren: () => import('./login/login.module').then(m => m.LoginModule)}
];

@NgModule({
    imports: [RouterModule.forRoot(routes)],
    exports: [RouterModule]
})
export class AppRoutingModule {
}
