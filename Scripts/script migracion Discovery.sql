USE [MVCommon]
GO

/****** Object:  StoredProcedure [dbo].[sp_crudEmailTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


create PROCEDURE [dbo].[sp_crudEmailTicket]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- Create date: <8/11/2016>
-- Description: <Gestiona (actualiza) EmailTickets >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      varchar(40)
    DECLARE @idCliente      varchar(40)
    DECLARE @mensaje      varchar(max)
    DECLARE @fechaEnvio      datetime
    DECLARE @idEstado      varchar(40)
    DECLARE @idTipo      int
    DECLARE @idTicket      varchar(40)
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','varchar(40)')
      ,@idCliente = doc.col.value ('idCliente[1]','varchar(40)')
      ,@mensaje = doc.col.value ('mensaje[1]','VARCHAR(max)')
      ,@fechaEnvio = doc.col.value ('fechaEnvio[1]','DATETIME')
      ,@idEstado = doc.col.value ('idEstado[1]','varchar(40)')
      ,@idTipo = doc.col.value ('idTipo[1]','INT')
      ,@idTicket = doc.col.value ('idTicket[1]','varchar(40)')

       FROM
       @xml.nodes('/ROOT/EmailTicketVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  EmailTicket WHERE   id=@id) ) 
       BEGIN
 
       	  INSERT INTO EmailTicket(
           id,
           idCliente,
           mensaje,
           fechaEnvio,
           idEstado,
           idTipo,
           idTicket)
           VALUES
            ( @id, @idCliente, @mensaje, GETDATE(), @idEstado, @idTipo, @idTicket) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @id=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El EmailTicket ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM EmailTicket WHERE  id=@id  ))
           BEGIN

			    UPDATE EmailTicket SET  

                   idCliente = @idCliente,
                   mensaje = @mensaje,
                   fechaEnvio = @fechaEnvio,
                   idEstado = @idEstado,
                   idTipo = @idTipo,
                   idTicket = @idTicket 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El EmailTicket NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE EmailTicket WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END

GO

/****** Object:  StoredProcedure [dbo].[sp_getListaViadeContacto]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE PROCEDURE [dbo].[sp_getListaViadeContacto] 

AS
-- =============================================
-- Description: <Obtiene una lista de regitros de ViadeContacto  >
-- ============================================= 

BEGIN

       SELECT id, descripcion, estado           FROM ViadeContacto ORDER BY Descripcion
END

GO

/****** Object:  StoredProcedure [dbo].[sp_getTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getTicket]
(
	@Id int 
)
AS
-- =============================================
-- Description: <Obtiene un regitro de Ticket por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT Ticket.Id, Ticket.FechaCreacion, Ticket.FechaModificacion, Ticket.IdAutor, Ticket.IdEmpleado, Ticket.Estado, Ticket.Seguimiento, Ticket.SeguimientoHistorico, Ticket.Asunto, Ticket.Descripcion, Ticket.IdPrioridad, 
                  Ticket.CodigoInterno, Ticket.CodigoCliente, Ticket.IdAgendamiento, Ticket.IdCliente, Ticket.IdSede, Ticket.IdContacto, Ticket.IdTipo, Ticket.IdEmpleadoModif, Ticket.FechaCierre, Estados.Descripcion AS EstadoNombre, 
                  Empresas.RazonSocial, Empleados.Nombre + ' ' + Empleados.Apellido AS Responsable, Empleados_1.Nombre + ' ' + Empleados_1.Apellido AS Autor, Ticket.IdDispositivo, Ticket.EstadoTicket
FROM     Estados RIGHT OUTER JOIN
                  Empleados RIGHT OUTER JOIN
                  Empresas RIGHT OUTER JOIN
                  Ticket ON Empresas.Id = Ticket.IdCliente ON Empleados.IdRow = Ticket.IdEmpleado ON Estados.Id = Ticket.Estado LEFT OUTER JOIN
                  Empleados AS Empleados_1 ON Ticket.IdAutor = Empleados_1.IdRow WHERE Ticket.CodigoInterno = @Id 
END



GO

/****** Object:  StoredProcedure [dbo].[sp_crudTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_crudTicket]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: DANIEL BASTIDAS
-- Create date: <3/05/2016>
-- Description: <Gestiona (actualiza) Tickets >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      varchar(40)
    DECLARE @fechaCreacion      datetime
    DECLARE @fechaModificacion      datetime
    DECLARE @idAutor      varchar(40)
    DECLARE @idEmpleado      varchar(40)
	 DECLARE @estadoTicket      varchar(40)
    DECLARE @estado      int
    DECLARE @seguimiento      varchar(max)
    DECLARE @seguimientoHistorico      varchar(max)
    DECLARE @asunto      varchar(500)
    DECLARE @descripcion      varchar(1000)
    DECLARE @idPrioridad      int
    DECLARE @codigoInterno      int
    DECLARE @codigoCliente      varchar(200)
    DECLARE @idAgendamiento      varchar(40)
    DECLARE @idCliente      varchar(40)
    DECLARE @idSede      varchar(40)
    DECLARE @idContacto      varchar(40)
    DECLARE @idTipo      int
    DECLARE @idEmpleadoModif      varchar(40)
    DECLARE @fechaCierre      datetime
	DECLARE @idDispositivo      varchar(40)
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','varchar(40)')
      ,@fechaCreacion = doc.col.value ('fechaCreacion[1]','DATETIME')
      ,@fechaModificacion = doc.col.value ('fechaModificacion[1]','DATETIME')
      ,@idAutor = doc.col.value ('idAutor[1]','varchar(40)')
      ,@idEmpleado = doc.col.value ('idEmpleado[1]','varchar(40)')
      ,@estado = doc.col.value ('estado[1]','INT')
      ,@seguimiento = doc.col.value ('seguimiento[1]','VARCHAR(max)')
      ,@seguimientoHistorico = doc.col.value ('seguimientoHistorico[1]','VARCHAR(max)')
      ,@asunto = doc.col.value ('asunto[1]','VARCHAR(500)')
      ,@descripcion = doc.col.value ('descripcion[1]','VARCHAR(1000)')
      ,@idPrioridad = doc.col.value ('idPrioridad[1]','INT')
      ,@codigoInterno = doc.col.value ('codigoInterno[1]','INT')
      ,@codigoCliente = doc.col.value ('codigoCliente[1]','VARCHAR(200)')
      ,@idAgendamiento = doc.col.value ('idAgendamiento[1]','varchar(40)')
      ,@idCliente = doc.col.value ('idCliente[1]','varchar(40)')
      ,@idSede = doc.col.value ('idSede[1]','varchar(40)')
      ,@idContacto = doc.col.value ('idContacto[1]','varchar(40)')
      ,@idTipo = doc.col.value ('idTipo[1]','INT')
      ,@idEmpleadoModif = doc.col.value ('idEmpleadoModif[1]','varchar(40)')
	  ,@idDispositivo = doc.col.value ('idDispositivo[1]','varchar(40)')
      ,@fechaCierre = doc.col.value ('fechaCierre[1]','DATETIME')
	  ,@estadoTicket = doc.col.value ('estadoTicket[1]','varchar(40)')

       FROM
       @xml.nodes('/ROOT/TicketVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  Ticket WHERE   id=@id) ) 
       BEGIN
 
       	  INSERT INTO Ticket(
           id,
           fechaCreacion,
           fechaModificacion,
           idAutor,
           idEmpleado,
           estado,
           seguimiento,
           seguimientoHistorico,
           asunto,
           descripcion,
           idPrioridad,
           codigoCliente,
           idAgendamiento,
           idCliente,
           idSede,
           idContacto,
           idTipo,
           idEmpleadoModif,
           fechaCierre,
		   idDispositivo, EstadoTicket)
           VALUES
            ( @id, @fechaCreacion, @fechaModificacion, @idAutor, @idEmpleado, @estado, @seguimiento, @seguimientoHistorico, @asunto, @descripcion, @idPrioridad, @codigoCliente, @idAgendamiento, @idCliente, @idSede, @idContacto, @idTipo, @idEmpleadoModif, @fechaCierre, @idDispositivo, @estadoTicket) 
			SET @Resultado=SCOPE_IDENTITY()
			

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El Ticket ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM Ticket WHERE  CodigoInterno=@codigoInterno	  ))
           BEGIN

			    UPDATE Ticket SET  

                   fechaCreacion = @fechaCreacion,
                   fechaModificacion = @fechaModificacion,
            
                   idEmpleado = @idEmpleado,
                   estado = @estado,
                   seguimiento = @seguimiento,
                   seguimientoHistorico = @seguimientoHistorico,
                   asunto = @asunto,
                   descripcion = @descripcion,
                   idPrioridad = @idPrioridad,
                   codigoCliente = @codigoCliente,
                   idAgendamiento = @idAgendamiento,
                   idCliente = @idCliente,
                   idSede = @idSede,
                   idContacto = @idContacto,
                   idTipo = @idTipo,
                   idEmpleadoModif = @idEmpleadoModif,
                   fechaCierre = @fechaCierre ,
				   idDispositivo = @idDispositivo,
				   estadoTicket = @estadoTicket
					 WHERE  CodigoInterno=@codigoInterno	

        END
			ELSE 
        BEGIN

					RAISERROR ('El Ticket NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE Ticket WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END
RETURN

GO

/****** Object:  StoredProcedure [dbo].[sp_getUsuariosSistema]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getUsuariosSistema]
(
	@Login varchar(100),
	@Password varchar(100)
)
AS
-- =============================================
-- Description: <Obtiene un regitro de UsuariosSistema por defecto toma su ID >
-- ============================================= 

BEGIN

   SELECT UsuariosSistema.Id, UsuariosSistema.Login, UsuariosSistema.Idgrupo, Empleados.Nombre, Empleados.Apellido, Empleados.FechaCreacion
FROM     UsuariosSistema INNER JOIN
                  Empleados ON UsuariosSistema.Id = Empleados.IdRow
        WHERE Login = @Login AND (DECRYPTBYPASSPHRASE('C1traseña', UsuariosSistema.Contraseña)) = @Password
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getTipoTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getTipoTicket]
(
	@Id int
)
AS
-- =============================================
-- Description: <Obtiene un regitro de TipoTicket por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT id, nombre, descripcion, estado
        FROM TipoTicket WHERE Id = @Id 
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getTareaDetalleTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getTareaDetalleTicket]
(
	@Id varchar(40)
)
AS
-- =============================================
-- Description: <Obtiene un regitro de TareaDetalleTicket por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT id, idDetalleTicket, tarea, orden, fechaCreacion, fechaInicio, fechaFinalizacion, 
          finalizado, seguimiento
        FROM TareaDetalleTicket WHERE Id = @Id 
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getProductos]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getProductos]
(
	@Id uniqueidentifier
)
AS
-- =============================================
-- Description: <Obtiene un regitro de Productos por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT id, idProveedor, numeroParte, idEmpleado, nombre, descripcion, idMarca, 
          grupoAsignado, imagen, caracteristicas, modelo, voltaje, hertz, fechaIngreso, 
          largo, ancho, altura, umDimencion, peso, umPeso, tipoRecogida, 
          comision, precioUnitario, iva, arancel, link, tipoMoneda, activo, 
          idcategoria, idtipoProducto, idClasificacion
        FROM Productos WHERE Id = @Id 
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getPrioridades]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getPrioridades]
(
	@Id int
)
AS
-- =============================================
-- Description: <Obtiene un regitro de Prioridades por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT id, descripcion, tiempoCumplimiento, estado
        FROM Prioridades WHERE Id = @Id 
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaUsuarioNotificacion]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getListaUsuarioNotificacion] 
(
	@Id varchar(40)
)
AS
-- =============================================
-- Description: <Obtiene una lista de regitros de UsuarioNotificacion  >
-- ============================================= 

BEGIN

       SELECT idUsuario, proceso, estado           FROM UsuarioNotificacion
	    WHERE proceso = @Id and Estado = 1
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getListaTipoTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaTipoTicket] 

AS
-- =============================================
-- Description: <Obtiene una lista de regitros de TipoTicket  >
-- ============================================= 

BEGIN

       SELECT id, nombre, descripcion, estado           FROM TipoTicket ORDER BY Nombre
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaTiempoSuscripcion]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaTiempoSuscripcion] 
(
	@Id varchar(40)
)
AS
-- =============================================
-- Description: <Obtiene un regitro de TiempoSuscripcion por defecto toma su ID >
-- ============================================= 

BEGIN
 SELECT TiempoSuscripcion.IdSuscripcion, TiempoSuscripcion.IdPlan, TiempoSuscripcion.IdTipoSoporte, TiempoSuscripcion.TiempoRestante, Suscripcion.Descripcion AS Suscripcion, 
                  ParametrizadorGeneral_1.Nombre AS NomPlan, ParametrizadorGeneral.Nombre AS TipoSoporte
FROM     TiempoSuscripcion INNER JOIN
                  Suscripcion ON TiempoSuscripcion.IdSuscripcion = Suscripcion.Id INNER JOIN
                  ParametrizadorGeneral AS ParametrizadorGeneral_1 ON TiempoSuscripcion.IdPlan = ParametrizadorGeneral_1.Id INNER JOIN
                  ParametrizadorGeneral ON TiempoSuscripcion.IdTipoSoporte = ParametrizadorGeneral.Id
				
WHERE  (Suscripcion.IdCliente = @Id)  AND FechaVencimiento > GETDATE() AND Suscripcion.Estado = 1
  order by Suscripcion, NomPlan, TipoSoporte
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaTicketBusqueda]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaTicketBusqueda] 
(
@IdUser varchar(40),
@CodigoEmpleado varchar(40),
@Estado int,
 @NombreCliente varchar(200),
 @CodigoInterno int,
  @CodigoCliente varchar(100),
  @fecha varchar(50),
  @prioridad int,
  @tipo int
)
AS
-- =============================================
-- Description: <Obtiene una lista de regitros de Ticket  >
-- ============================================= 

BEGIN

SELECT DISTINCT 
                  Ticket.Id, Ticket.FechaCreacion, Ticket.FechaModificacion, Ticket.IdAutor, Ticket.IdEmpleado, Ticket.Estado, Ticket.Seguimiento, Ticket.SeguimientoHistorico, Ticket.Asunto, Ticket.Descripcion, Ticket.IdPrioridad, 
                  Ticket.CodigoInterno, Ticket.CodigoCliente, Ticket.IdAgendamiento, Ticket.IdCliente, Ticket.IdSede, Ticket.IdContacto, Ticket.IdTipo, Ticket.IdEmpleadoModif, Ticket.FechaCierre, Estados.Descripcion AS EstadoNombre, 
                  Empresas.RazonSocial, Asesor.Nombre + ' ' + Asesor.Apellido AS Responsable, Autor.Nombre + ' ' + Autor.Apellido AS Autor, Prioridades.Descripcion AS Prioridad

FROM     Prioridades RIGHT OUTER JOIN
                  Empleados AS Asesor RIGHT OUTER JOIN
                  Ticket LEFT OUTER JOIN
                  UsuariosGrupos INNER JOIN
                  GruposProceso ON UsuariosGrupos.IdGrupo = GruposProceso.IdGrupo ON Ticket.Id = GruposProceso.IdProceso LEFT OUTER JOIN
                  Estados ON Ticket.Estado = Estados.Id ON Asesor.IdRow = Ticket.IdEmpleado LEFT OUTER JOIN
                  Empresas ON Ticket.IdCliente = Empresas.Id ON Prioridades.Id = Ticket.IdPrioridad LEFT OUTER JOIN
                  Empleados AS Autor ON Ticket.IdAutor = Autor.IdRow LEFT OUTER JOIN
                  SedesClientes ON Ticket.IdSede = SedesClientes.Id
WHERE (Ticket.Estado = @Estado) AND (UsuariosGrupos.IdUser = @IdUser) AND (Asesor.IdRow = @CodigoEmpleado OR
                  @CodigoEmpleado = '00000000-0000-0000-0000-000000000000') AND (Empresas.Id LIKE '%' + @NombreCliente + '%' OR
                  @NombreCliente = '') AND (Empresas.Identificacion LIKE '%' + @CodigoCliente + '%' OR
                  @CodigoCliente = '') AND (Ticket.CodigoInterno LIKE '%' + CAST(@CodigoInterno AS VARCHAR(65)) + '%' OR
                  @CodigoInterno = - 1) AND (CONVERT(CHAR(10), GETDATE(), 101) = @fecha or @fecha = ''  )
				  AND (Ticket.IdPrioridad = @prioridad OR @prioridad = 0)
				  AND (Ticket.IdTipo = @tipo OR @tipo =0)
				  ORDER BY CodigoInterno desc

END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getListaTicket] 
(
@IdUser uniqueidentifier,
@Tipo varchar(5) 
)
AS
-- =============================================
-- Description: <Obtiene una lista de regitros de Ticket  >
-- ============================================= 

BEGIN
IF(@Tipo LIKE '1')
begin
SELECT DISTINCT 
                  Ticket.Id, Ticket.FechaCreacion, Ticket.FechaModificacion, Ticket.IdAutor, Ticket.IdEmpleado, Ticket.Estado, Ticket.Seguimiento, Ticket.SeguimientoHistorico, Ticket.Asunto, Ticket.Descripcion, Ticket.IdPrioridad, 
                  Ticket.CodigoInterno, Ticket.CodigoCliente, Ticket.IdAgendamiento, Ticket.IdCliente, Ticket.IdSede, Ticket.IdContacto, Ticket.IdTipo, Ticket.IdEmpleadoModif, Ticket.FechaCierre, Estados.Descripcion AS EstadoNombre, 
                  Empresas.RazonSocial, Asesor.Nombre + ' ' + Asesor.Apellido AS Responsable, Autor.Nombre + ' ' + Autor.Apellido AS Autor, Prioridades.Descripcion AS Prioridad
FROM     Prioridades RIGHT OUTER JOIN
                  Empleados AS Asesor RIGHT OUTER JOIN
                  Ticket LEFT OUTER JOIN
                  UsuariosGrupos INNER JOIN
                  GruposProceso ON UsuariosGrupos.IdGrupo = GruposProceso.IdGrupo ON Ticket.Id = GruposProceso.IdProceso LEFT OUTER JOIN
                  Estados ON Ticket.Estado = Estados.Id ON Asesor.IdRow = Ticket.IdEmpleado LEFT OUTER JOIN
                  Empresas ON Ticket.IdCliente = Empresas.Id ON Prioridades.Id = Ticket.IdPrioridad LEFT OUTER JOIN
                  Empleados AS Autor ON Ticket.IdAutor = Autor.IdRow LEFT OUTER JOIN
                  SedesClientes ON Ticket.IdSede = SedesClientes.Id
WHERE Ticket.Estado=1 AND UsuariosGrupos.IdUser=@IdUser
				  ORDER BY CodigoInterno desc
END
				  ELSE 
				  BEGIN
				  SELECT DISTINCT 
                  Ticket.Id, Ticket.FechaCreacion, Ticket.FechaModificacion, Ticket.IdAutor, Ticket.IdEmpleado, Ticket.Estado, Ticket.Seguimiento, Ticket.SeguimientoHistorico, Ticket.Asunto, Ticket.Descripcion, Ticket.IdPrioridad, 
                  Ticket.CodigoInterno, Ticket.CodigoCliente, Ticket.IdAgendamiento, Ticket.IdCliente, Ticket.IdSede, Ticket.IdContacto, Ticket.IdTipo, Ticket.IdEmpleadoModif, Ticket.FechaCierre, Estados.Descripcion AS EstadoNombre, 
                  Empresas.RazonSocial, Asesor.Nombre + ' ' + Asesor.Apellido AS Responsable, Autor.Nombre + ' ' + Autor.Apellido AS Autor, Prioridades.Descripcion AS Prioridad
FROM     Prioridades RIGHT OUTER JOIN
                  Empleados AS Asesor RIGHT OUTER JOIN
                  Ticket LEFT OUTER JOIN
                  UsuariosGrupos INNER JOIN
                  GruposProceso ON UsuariosGrupos.IdGrupo = GruposProceso.IdGrupo ON Ticket.Id = GruposProceso.IdProceso LEFT OUTER JOIN
                  Estados ON Ticket.Estado = Estados.Id ON Asesor.IdRow = Ticket.IdEmpleado LEFT OUTER JOIN
                  Empresas ON Ticket.IdCliente = Empresas.Id ON Prioridades.Id = Ticket.IdPrioridad LEFT OUTER JOIN
                  Empleados AS Autor ON Ticket.IdAutor = Autor.IdRow LEFT OUTER JOIN
                  SedesClientes ON Ticket.IdSede = SedesClientes.Id
				  WHERE Empresas.Id = @IdUser
				  	  ORDER BY CodigoInterno desc
				  END
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getListaTareaDetalleTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaTareaDetalleTicket] 
(
	@Id varchar(40)
)
AS
-- =============================================
-- Description: <Obtiene una lista de regitros de TareaDetalleTicket  >
-- ============================================= 

BEGIN

SELECT TareaDetalleTicket.Id, TareaDetalleTicket.IdDetalleTicket, TareaDetalleTicket.Tarea, TareaDetalleTicket.Orden, TareaDetalleTicket.FechaCreacion, TareaDetalleTicket.FechaInicio, TareaDetalleTicket.FechaFinalizacion, 
                  TareaDetalleTicket.Finalizado, TareaDetalleTicket.Seguimiento, TareaDetalleTicket.IdTipoSoporte, DetalletTicket.IdSuscripcion, Suscripcion.IdPlan, CONVERT(varchar(10), TareaDetalleTicket.Orden) 
                  + ' ' + isnull(Suscripcion.Descripcion,'')+': '+Productos.Nombre AS Suscripcion, TareaDetalleTicket.Segundos, Productos.Nombre AS Producto
FROM     Productos RIGHT OUTER JOIN
                  DetalletTicket ON Productos.Id = DetalletTicket.IdServicio LEFT OUTER JOIN
                  Suscripcion ON DetalletTicket.IdSuscripcion = Suscripcion.Id LEFT OUTER JOIN
                  TareaDetalleTicket ON DetalletTicket.Id = TareaDetalleTicket.IdDetalleTicket
		   WHERE DetalletTicket.IdTicket = @Id 
		  ORDER BY Producto, Suscripcion, Orden
END




GO

/****** Object:  StoredProcedure [dbo].[sp_getListaSuscripcion]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





CREATE PROCEDURE [dbo].[sp_getListaSuscripcion]
(
	@Id uniqueidentifier
)
AS
-- =============================================
-- Description: <Obtiene un regitro de Suscripcion por defecto toma su ID >
-- ============================================= 

BEGIN


       SELECT id, descripcion, fechaCreacion, fechaInicio, fechaVencimiento, idCliente, idPlan, 
          idTipoRenovacion, estado, codigo
        FROM Suscripcion WHERE idCliente = @Id AND FechaVencimiento > GETDATE() AND Estado = 1
END




GO

/****** Object:  StoredProcedure [dbo].[sp_getListaReportes]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getListaReportes] 

AS
-- =============================================
-- Description: <Obtiene una lista de regitros de Reportes  >
-- ============================================= 

BEGIN

       SELECT id, nombreModulo, nombreReporteMostrar, nombreReporteReal, nombreParametroCodigo, nombreParametro1, activo
          
                     FROM Reportes
					 where activo= 1
					  ORDER BY NombreModulo, nombreReporteMostrar

END


GO

/****** Object:  StoredProcedure [dbo].[sp_getListaProductosTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getListaProductosTicket]
(
	@Id varchar(40)
)
AS
-- =============================================
-- Description: <Obtiene un regitro de ProductosTicket por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT id, idTicket, producto, numeroParte, valor
        FROM ProductosTicket WHERE idTicket = @Id 
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getListaProductos]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaProductos] 

AS
-- =============================================
-- Description: <Obtiene una lista de regitros de Productos  >
-- ============================================= 

BEGIN

       SELECT id, idProveedor, numeroParte, idEmpleado, nombre, descripcion, idMarca, 
          grupoAsignado, imagen, caracteristicas, modelo, voltaje, hertz, fechaIngreso, 
          largo, ancho, altura, umDimencion, peso, umPeso, tipoRecogida, 
          comision, precioUnitario, iva, arancel, link, tipoMoneda, activo, 
          idcategoria, idtipoProducto, idClasificacion           FROM Productos 
		  WHERE idClasificacion = 3 AND activo = 1 ORDER BY Nombre
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaPrioridades]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaPrioridades] 

AS
-- =============================================
-- Description: <Obtiene una lista de regitros de Prioridades  >
-- ============================================= 

BEGIN

       SELECT id, descripcion, tiempoCumplimiento, estado           FROM Prioridades ORDER BY Descripcion
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaPasoServicio]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaPasoServicio] 
(
@IdProducto varchar(40)
)
AS
-- =============================================
-- Description: <Obtiene una lista de regitros de DatosGenerales  >
-- ============================================= 

BEGIN
SELECT      Id,  Descripcion, Orden
FROM            PasoServicio
WHERE        (IdProducto = @IdProducto)
ORDER BY Orden

END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaParametrizadorGeneral]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaParametrizadorGeneral] 
(
	@Id varchar(40)
)
AS
-- =============================================
-- Description: <Obtiene un regitro de TiempoSuscripcion por defecto toma su ID >
-- ============================================= 

BEGIN
SELECT Id, Nombre, Descripcion
FROM     ParametrizadorGeneral
where IdTipoParametrizador = @Id AND Estado = 1
ORDER BY Nombre
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaHistorial]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



create PROCEDURE [dbo].[sp_getListaHistorial] 
(
@Id varchar(40)
)
AS
-- =============================================
-- Description: <Obtiene una lista de regitros de Historial  >
-- ============================================= 

BEGIN

       SELECT id, fecha, idEmpleado, descripcion, tipo, extension, documento, 
          idReferencia           FROM Historial 
		  where IdReferencia = @Id
		  ORDER BY Descripcion
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getListaEstado]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getListaEstado] 

AS
-- =============================================
-- Description: <Obtiene una lista de regitros de Ticket  >
-- ============================================= 

BEGIN
SELECT Id, Descripcion
FROM     Estados
where Id in (1,2,3)
order by Descripcion
    
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getListaGruposTrabajo]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaGruposTrabajo] 
(
	 @Id uniqueidentifier 
)
AS
-- =============================================
-- Description: <Obtiene una lista de regitros de GruposTrabajo  >
-- ============================================= 

BEGIN

      SELECT DISTINCT GruposTrabajo.id, GruposTrabajo.Nombre, GruposTrabajo.Sigla, GruposTrabajo.DescripcionActividad
FROM     GruposTrabajo LEFT OUTER JOIN
                  GruposProceso ON GruposTrabajo.id = GruposProceso.IdGrupo
				  WHERE GruposProceso.IdProceso = @Id OR @Id = '00000000-0000-0000-0000-000000000000'

ORDER BY GruposTrabajo.Nombre
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaEncuentasTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaEncuentasTicket] 
(
	@Id varchar(40)
)
AS
-- =============================================
-- Description: <Obtiene una lista de regitros de EncuentasTicket  >
-- ============================================= 

BEGIN

    SELECT    DISTINCT   ParametrizadorGeneral.Id, ParametrizadorGeneral.Nombre, ISNULL(Et.Respuesta,-1) As Respuesta, Et.IdTicket, et.IdPregunta
FROM            ParametrizadorGeneral LEFT OUTER JOIN
                        ( SELECT  DISTINCT * FROM   EncuentasTicket WHERE EncuentasTicket.IdTicket=@Id ) Et ON ParametrizadorGeneral.Id = Et.IdPregunta
WHERE        (ParametrizadorGeneral.IdTipoParametrizador = 13) 
	    ORDER BY IdTicket

END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaEmpresas]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getListaEmpresas] 
(
@param varchar(150)
)
AS
-- =============================================
-- Description: <Obtiene una lista de regitros de Empresas  >
-- ============================================= 

BEGIN

       SELECT top 50 id, razonSocial           FROM Empresas
		  where (RazonSocial like '%'+@param+'%' OR Identificacion like '%'+@param+'%' )
		  AND Estado = 1
		   ORDER BY RazonSocial
		
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getListaEmpleados]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getListaEmpleados]

AS
-- =============================================
-- Description: <Obtiene un regitro de Empleados por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT idRow, email, nombre + ' '+apellido as nombreCompleto
        FROM Empleados where est=1 
		UNION
		select '00000000-0000-0000-0000-000000000000', '', ' Todos'
		ORDER BY nombreCompleto
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getListaDispositivo]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getListaDispositivo]
(
	@IdCliente uniqueidentifier ,
	@IdSusc uniqueidentifier 
)
AS
-- =============================================
-- Description: <Obtiene un regitro de Dispositivo por defecto toma su ID >
-- ============================================= 

BEGIN

if (@IdSusc <> '00000000-0000-0000-0000-000000000000')
BEGIN
  SELECT distinct Dispositivo.Id, '' AS Codigo, Dispositivo.Descripcion + ': '+isnull(Suscripcion.Descripcion,'')  AS Descripcion, SuscripcionDispositivo.IdSuscripcion
FROM     Suscripcion RIGHT OUTER JOIN
                  SuscripcionDispositivo ON Suscripcion.Id = SuscripcionDispositivo.IdSuscripcion RIGHT OUTER JOIN
                  Dispositivo ON SuscripcionDispositivo.IdDispositivo = Dispositivo.Id
WHERE  (Dispositivo.IdCliente = @IdCliente) AND (SuscripcionDispositivo.IdSuscripcion = @IdSusc)  AND FechaVencimiento > GETDATE() AND Dispositivo.Estado = 1
AND Suscripcion.Estado = 1
END
ELSE
BEGIN
  SELECT distinct Dispositivo.Id, '' AS Codigo, Dispositivo.Descripcion   AS Descripcion
FROM    (select * from Suscripcion where Suscripcion.Estado =1  AND FechaVencimiento > GETDATE()) AS Suscripcion RIGHT OUTER JOIN
                  SuscripcionDispositivo ON Suscripcion.Id = SuscripcionDispositivo.IdSuscripcion RIGHT OUTER JOIN
                  Dispositivo ON SuscripcionDispositivo.IdDispositivo = Dispositivo.Id
WHERE  (Dispositivo.IdCliente = @IdCliente) AND Dispositivo.Estado = 1 

END
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getListaDetalletTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaDetalletTicket]
(
	@Id uniqueidentifier
)
AS
-- =============================================
-- Description: <Obtiene un regitro de DetalletTicket por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT DetalletTicket.Id, DetalletTicket.IdTicket, DetalletTicket.IdServicio, DetalletTicket.IdDispositivo, DetalletTicket.IdSuscripcion, DetalletTicket.IdTipo, DetalletTicket.Descripcion, Productos.Nombre AS Servicio, 
                  Dispositivo.Descripcion AS Dispositivo, Suscripcion.Descripcion AS Suscripcion
FROM     DetalletTicket LEFT OUTER JOIN
                  Suscripcion ON DetalletTicket.IdSuscripcion = Suscripcion.Id LEFT OUTER JOIN
                  Dispositivo ON DetalletTicket.IdDispositivo = Dispositivo.Id LEFT OUTER JOIN
                  Productos ON DetalletTicket.IdServicio = Productos.Id WHERE idTicket = @Id 
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getListaDatosGenerales]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getListaDatosGenerales] 

AS
-- =============================================
-- Description: <Obtiene una lista de regitros de DatosGenerales  >
-- ============================================= 

BEGIN

       SELECT id, nombreEmpresa, stringconexion, stringReport, mail_smtp, 
          mail_Puerto, mailCredencialUser, mailCredencialpass, mail_SSL, textFooter, direccion, telefono, 
          mail, fax, factorVenta, factorVentasEnporciento, permitirVerCostos, permitirModificarCostos, permitirAsesoresClientes, 
          permitirVerUtilidad, permitirModUtilidad, utilidadSobreCosto, color, monedafuncional, usuariosSistema, version, 
          aiu, direccionFTP, usuarioFTP, contraseñaFTP, deshabilitarProductos, agendamientosEnPedido, sedesContactos, 
          integracionErp, validarSede, cierreProcesoAutor, contactoObligatorio, cargoServicios, grupoProductos, servicioGrupo, 
          integracionMultifox, sincronizarOutlook, tiempoSincronizacion, asuntoObligatorio, cargoTickets, mailCumpleaños, 
          mensajeCumple, indice, productosOport , GrupoTickets          FROM DatosGenerales ORDER BY NombreEmpresa
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getListaAgenda]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getListaAgenda] 
(
		@Id varchar(40)
	)
AS
-- =============================================
-- Description: <Obtiene una lista de regitros de Agenda  >
-- ============================================= 

BEGIN

       SELECT idRow, asunto, fechaElaboracion, fechainicio, fechaFin, descripcion, seguimiento, 
          idReferencia, responsable, idViacontacto, idAutor, estado, fechaCierre, recordatorio, 
          programada, orden
          programada, orden           FROM Agenda 
		  WHERE idReferencia = @Id 
		  ORDER BY FechaElaboracion
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getHistorial]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getHistorial]
(
	@Id varchar (40)
)
AS
-- =============================================
-- Description: <Obtiene un regitro de Historial por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT id, fecha, idEmpleado, descripcion, tipo, extension, documento, 
          idReferencia
        FROM Historial WHERE Id = @Id 
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getGruposTrabajo]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getGruposTrabajo]
(
	@Id int
)
AS
-- =============================================
-- Description: <Obtiene un regitro de GruposTrabajo por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT id, nombre, sigla, descripcionActividad
        FROM GruposTrabajo WHERE id = @Id 
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getEmpresas]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getEmpresas]
(
	@correo varchar(150),
	@nit varchar(150)
)
AS
-- =============================================
-- Description: <Obtiene un regitro de Empresas por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT id, tipoEmpresa, identificacion, dv, razonSocial, antiguedad, telefono, 
          fax, direccion, mail, url, ciudad, departamento, pais, 
          aniversario, tieneAniversario, fechaIngreso, idActividadEconomica, movil, observaciones, seguimientoHistorico, 
          estado, idAsesor, representanteLegal, idTipoMonedaImportacion, tipoNacionalidad, idEmpleadoModif, codigoInterno, 
          idResponsableFacturacion, idResponsableServicio
        FROM Empresas WHERE mail = @correo AND Identificacion = @nit 
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getEmpresa]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



create PROCEDURE [dbo].[sp_getEmpresa]
(
	@Id varchar(150)
	)
AS
-- =============================================
-- Description: <Obtiene un regitro de Empresas por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT id, tipoEmpresa, identificacion, dv, razonSocial, antiguedad, telefono, 
          fax, direccion, mail, url, ciudad, departamento, pais, 
          aniversario, tieneAniversario, fechaIngreso, idActividadEconomica, movil, observaciones, seguimientoHistorico, 
          estado, idAsesor, representanteLegal, idTipoMonedaImportacion, tipoNacionalidad, idEmpleadoModif, codigoInterno, 
          idResponsableFacturacion, idResponsableServicio
        FROM Empresas WHERE Id = @Id
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getEmpleados2]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getEmpleados2]
(
	@Id varchar(50)
)
AS
-- =============================================
-- Description: <Obtiene un regitro de Empleados por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT top 1 idRow, codigo, codigo_interno, nombre, apellido, email, proceso, 
          telefono, celular, est, fechaCreacion, signature, tamano, fechaContratacion, 
          fechanacimiento, rh, idCargo, idOcupacion, direccion, idEmpresa, minNotificaciones, 
          emailPassword, idCiudad, esJefe, grupoEncargado
        FROM Empleados WHERE idRow = @Id 
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getEmpleados]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_getEmpleados]
(
	@Id int
)
AS
-- =============================================
-- Description: <Obtiene un regitro de Empleados por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT top 1 idRow, codigo, codigo_interno, nombre, apellido, email, proceso, 
          telefono, celular, est, fechaCreacion, signature, tamano, fechaContratacion, 
          fechanacimiento, rh, idCargo, idOcupacion, direccion, idEmpresa, minNotificaciones, 
          emailPassword, idCiudad, esJefe, grupoEncargado
        FROM Empleados WHERE Orden = @Id 
END


GO

/****** Object:  StoredProcedure [dbo].[sp_getDetalletTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getDetalletTicket]
(
	@Id uniqueidentifier
)
AS
-- =============================================
-- Description: <Obtiene un regitro de DetalletTicket por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT DetalletTicket.Id, DetalletTicket.IdTicket, DetalletTicket.IdServicio, DetalletTicket.IdDispositivo, DetalletTicket.IdSuscripcion, DetalletTicket.IdTipo, DetalletTicket.Descripcion, Productos.Nombre AS Servicio, 
                  Dispositivo.Descripcion AS Dispositivo, Suscripcion.Descripcion AS Suscripcion
FROM     DetalletTicket LEFT OUTER JOIN
                  Suscripcion ON DetalletTicket.IdSuscripcion = Suscripcion.Id LEFT OUTER JOIN
                  Dispositivo ON DetalletTicket.IdDispositivo = Dispositivo.Id LEFT OUTER JOIN
                  Productos ON DetalletTicket.IdServicio = Productos.Id WHERE DetalletTicket.Id = @Id 
END



GO

/****** Object:  StoredProcedure [dbo].[sp_getAgenda]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_getAgenda]
(
	@Id varchar(40)
)
AS
-- =============================================
-- Description: <Obtiene un regitro de Agenda por defecto toma su ID >
-- ============================================= 

BEGIN

       SELECT idRow, asunto, fechaElaboracion, fechainicio, fechaFin, descripcion, seguimiento, 
          idReferencia, responsable, idViacontacto, idAutor, estado, fechaCierre, recordatorio, 
          programada, orden
        FROM Agenda WHERE IdRow = @Id 
END



GO

/****** Object:  StoredProcedure [dbo].[sp_crudUsuariosSistema]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudUsuariosSistema]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <11/04/2016>
-- Description: <Gestiona (actualiza) UsuariosSistemas >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      uniqueidentifier
    DECLARE @login      varchar(50)
    DECLARE @contraseña      varchar(50)
    DECLARE @idgrupo      int
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','UNIQUEIDENTIFIER')
      ,@login = doc.col.value ('login[1]','VARCHAR(50)')
      ,@contraseña = doc.col.value ('contraseña[1]','VARCHAR(50)')
      ,@idgrupo = doc.col.value ('idgrupo[1]','INT')

       FROM
       @xml.nodes('/ROOT/UsuariosSistemaVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  UsuariosSistema WHERE   id=@id) ) 
       BEGIN
 
       	  INSERT INTO UsuariosSistema(
           id,
           login,
           contraseña,
           idgrupo)
           VALUES
            ( @id, @login, @contraseña, @idgrupo) 
			SET @Resultado=SCOPE_IDENTITY()

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El UsuariosSistema ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM UsuariosSistema WHERE  id=@id  ))
           BEGIN

			    UPDATE UsuariosSistema SET  

                   login = @login,
                   contraseña = @contraseña,
                   idgrupo = @idgrupo 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El UsuariosSistema NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE UsuariosSistema WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END



GO

/****** Object:  StoredProcedure [dbo].[sp_crudTipoTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudTipoTicket]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <31/05/2016>
-- Description: <Gestiona (actualiza) TipoTickets >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      int
    DECLARE @nombre      varchar(150)
    DECLARE @descripcion      varchar(150)
    DECLARE @estado      bit
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','INT')
      ,@nombre = doc.col.value ('nombre[1]','VARCHAR(150)')
      ,@descripcion = doc.col.value ('descripcion[1]','VARCHAR(150)')
      ,@estado = doc.col.value ('estado[1]','BIT')

       FROM
       @xml.nodes('/ROOT/TipoTicketVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  TipoTicket WHERE   id=@id) ) 
       BEGIN
 
       	  INSERT INTO TipoTicket(
           id,
           nombre,
           descripcion,
           estado)
           VALUES
            ( @id, @nombre, @descripcion, @estado) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @id=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El TipoTicket ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM TipoTicket WHERE  id=@id  ))
           BEGIN

			    UPDATE TipoTicket SET  

                   nombre = @nombre,
                   descripcion = @descripcion,
                   estado = @estado 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El TipoTicket NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE TipoTicket WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END



GO

/****** Object:  StoredProcedure [dbo].[sp_crudTiempoSuscripcion]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudTiempoSuscripcion]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <17/06/2016>
-- Description: <Gestiona (actualiza) TiempoSuscripcions >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @idSuscripcion      varchar(40)
    DECLARE @idPlan      varchar(40)
    DECLARE @idTipoSoporte      varchar(40)
    DECLARE @tiempoRestante      int
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @idSuscripcion = doc.col.value ('idSuscripcion[1]','varchar(40)')
      ,@idPlan = doc.col.value ('idPlan[1]','varchar(40)')
      ,@idTipoSoporte = doc.col.value ('idTipoSoporte[1]','varchar(40)')
      ,@tiempoRestante = doc.col.value ('tiempoRestante[1]','INT')

       FROM
       @xml.nodes('/ROOT/TiempoSuscripcionVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  TiempoSuscripcion WHERE   idSuscripcion=@idSuscripcion) OR 1=1) 
       BEGIN
 
       	  INSERT INTO TiempoSuscripcion(
           idSuscripcion,
           idPlan,
           idTipoSoporte,
           tiempoRestante)
           VALUES
            ( @idSuscripcion, @idPlan, @idTipoSoporte, @tiempoRestante) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @idSuscripcion=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El TiempoSuscripcion ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM TiempoSuscripcion WHERE  idSuscripcion=@idSuscripcion  ) OR 1=1)
           BEGIN

			    UPDATE TiempoSuscripcion SET  
                   tiempoRestante = @tiempoRestante 
					WHERE  idSuscripcion=@idSuscripcion 	AND   idPlan = @idPlan AND
                   idTipoSoporte = @idTipoSoporte

        END
			ELSE 
        BEGIN

					RAISERROR ('El TiempoSuscripcion NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE TiempoSuscripcion WHERE idSuscripcion=@idSuscripcion
		

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END



GO

/****** Object:  StoredProcedure [dbo].[sp_crudTareaDetalleTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_crudTareaDetalleTicket]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- Create date: <20/06/2016>
-- Description: <Gestiona (actualiza) TareaDetalleTickets >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      varchar(40)
    DECLARE @idDetalleTicket      varchar(40)
	   DECLARE @segundos     varchar(150)
    DECLARE @tarea      varchar(max)
    DECLARE @orden      int
    DECLARE @fechaCreacion      datetime
    DECLARE @fechaInicio      datetime
    DECLARE @fechaFinalizacion      datetime
    DECLARE @finalizado      int
    DECLARE @seguimiento      varchar(max)
    DECLARE @idTipoSoporte      varchar(40)
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','varchar(40)')
      ,@idDetalleTicket = doc.col.value ('idDetalleTicket[1]','varchar(40)')
      ,@tarea = doc.col.value ('tarea[1]','VARCHAR(max)')
      ,@orden = doc.col.value ('orden[1]','INT')
      ,@fechaCreacion = doc.col.value ('fechaCreacion[1]','DATETIME')
      ,@fechaInicio = doc.col.value ('fechaInicio[1]','DATETIME')
      ,@fechaFinalizacion = doc.col.value ('fechaFinalizacion[1]','DATETIME')
      ,@finalizado = doc.col.value ('finalizado[1]','INT')
      ,@seguimiento = doc.col.value ('seguimiento[1]','VARCHAR(max)')
      ,@idTipoSoporte = doc.col.value ('idTipoSoporte[1]','varchar(40)')
	  ,@segundos = doc.col.value ('segundos[1]','varchar(150)')
       FROM
       @xml.nodes('/ROOT/TareaDetalleTicketVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  TareaDetalleTicket WHERE   id=@id) OR 1=1) 
       BEGIN
 
       	  INSERT INTO TareaDetalleTicket(
           id,
           idDetalleTicket,
           tarea,
           orden,
           fechaCreacion,
           fechaInicio,
           fechaFinalizacion,
           finalizado,
           seguimiento,
           idTipoSoporte, segundos)
           VALUES
            ( @id, @idDetalleTicket, @tarea, @orden, @fechaCreacion, @fechaInicio, @fechaFinalizacion, @finalizado, @seguimiento, @idTipoSoporte, @segundos) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @id=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El TareaDetalleTicket ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM TareaDetalleTicket WHERE  id=@id  ))
           BEGIN
		   if(@tarea = '2')
		   begin 
		   UPDATE TareaDetalleTicket SET  

                   seguimiento = @seguimiento
					WHERE  id=@id
		   end
		   else 
		   begin
		   UPDATE TareaDetalleTicket SET  

                   fechaInicio = @fechaInicio,
                   fechaFinalizacion = @fechaFinalizacion,
                   finalizado = @finalizado,
                   idTipoSoporte = @idTipoSoporte ,
				   segundos = @segundos
					WHERE  id=@id
		   end

			    

        END
			ELSE 
        BEGIN

					RAISERROR ('El TareaDetalleTicket NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE TareaDetalleTicket WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END
return

GO

/****** Object:  StoredProcedure [dbo].[sp_crudSuscripcion]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudSuscripcion]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <17/05/2016>
-- Description: <Gestiona (actualiza) Suscripcions >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      uniqueidentifier
    DECLARE @descripcion      varchar(200)
    DECLARE @fechaCreacion      datetime
    DECLARE @fechaInicio      datetime
    DECLARE @fechaVencimiento      datetime
    DECLARE @idCliente      uniqueidentifier
    DECLARE @idPlan      uniqueidentifier
    DECLARE @idTipoRenovacion      uniqueidentifier
    DECLARE @estado      bit
    DECLARE @codigo      int
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','UNIQUEIDENTIFIER')
      ,@descripcion = doc.col.value ('descripcion[1]','VARCHAR(200)')
      ,@fechaCreacion = doc.col.value ('fechaCreacion[1]','DATETIME')
      ,@fechaInicio = doc.col.value ('fechaInicio[1]','DATETIME')
      ,@fechaVencimiento = doc.col.value ('fechaVencimiento[1]','DATETIME')
      ,@idCliente = doc.col.value ('idCliente[1]','UNIQUEIDENTIFIER')
      ,@idPlan = doc.col.value ('idPlan[1]','UNIQUEIDENTIFIER')
      ,@idTipoRenovacion = doc.col.value ('idTipoRenovacion[1]','UNIQUEIDENTIFIER')
      ,@estado = doc.col.value ('estado[1]','BIT')
      ,@codigo = doc.col.value ('codigo[1]','INT')

       FROM
       @xml.nodes('/ROOT/SuscripcionVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  Suscripcion WHERE   id=@id) ) 
       BEGIN
 
       	  INSERT INTO Suscripcion(
           id,
           descripcion,
           fechaCreacion,
           fechaInicio,
           fechaVencimiento,
           idCliente,
           idPlan,
           idTipoRenovacion,
           estado
           )
           VALUES
            ( @id, @descripcion, @fechaCreacion, @fechaInicio, @fechaVencimiento, @idCliente, @idPlan, @idTipoRenovacion, @estado) 
			SET @Resultado=SCOPE_IDENTITY()

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El Suscripcion ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM Suscripcion WHERE  id=@id  ))
           BEGIN

			    UPDATE Suscripcion SET  

                   descripcion = @descripcion,
                   fechaCreacion = @fechaCreacion,
                   fechaInicio = @fechaInicio,
                   fechaVencimiento = @fechaVencimiento,
                   idCliente = @idCliente,
                   idPlan = @idPlan,
                   idTipoRenovacion = @idTipoRenovacion,
                   estado = @estado
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El Suscripcion NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE Suscripcion WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END



GO

/****** Object:  StoredProcedure [dbo].[sp_crudProductosTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_crudProductosTicket]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- Create date: <28/06/2016>
-- Description: <Gestiona (actualiza) ProductosTickets >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      varchar(40)
    DECLARE @idTicket      varchar(40)
    DECLARE @producto      varchar(250)
    DECLARE @numeroParte      varchar(250)
    DECLARE @valor      decimal
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','varchar(40)')
      ,@idTicket = doc.col.value ('idTicket[1]','varchar(40)')
      ,@producto = doc.col.value ('producto[1]','VARCHAR(250)')
      ,@numeroParte = doc.col.value ('numeroParte[1]','VARCHAR(250)')
      ,@valor = doc.col.value ('valor[1]','DECIMAL')

       FROM
       @xml.nodes('/ROOT/ProductosTicketVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  ProductosTicket WHERE   id=@id) OR 1=1) 
       BEGIN
 
       	  INSERT INTO ProductosTicket(
           id,
           idTicket,
           producto,
           numeroParte,
           valor)
           VALUES
            ( @id, @idTicket, @producto, @numeroParte, @valor) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @id=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El ProductosTicket ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM ProductosTicket WHERE  id=@id  ))
           BEGIN

			    UPDATE ProductosTicket SET  

                   idTicket = @idTicket,
                   producto = @producto,
                   numeroParte = @numeroParte,
                   valor = @valor 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El ProductosTicket NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE ProductosTicket WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END
RETURN

GO

/****** Object:  StoredProcedure [dbo].[sp_crudProductos]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudProductos]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <18/05/2016>
-- Description: <Gestiona (actualiza) Productoss >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      uniqueidentifier
    DECLARE @idProveedor      uniqueidentifier
    DECLARE @numeroParte      varchar(50)
    DECLARE @idEmpleado      uniqueidentifier
    DECLARE @nombre      varchar(MAX)
    DECLARE @descripcion      varchar(MAX)
    DECLARE @idMarca      int
    DECLARE @grupoAsignado      int
    DECLARE @caracteristicas      varchar(MAX)
    DECLARE @modelo      varchar(300)
    DECLARE @voltaje      varchar(300)
    DECLARE @hertz      varchar(300)
    DECLARE @fechaIngreso      datetime
    DECLARE @largo      decimal
    DECLARE @ancho      decimal
    DECLARE @altura      decimal
    DECLARE @umDimencion      int
    DECLARE @peso      decimal
    DECLARE @umPeso      int
    DECLARE @tipoRecogida      varchar(300)
    DECLARE @comision      varchar(300)
    DECLARE @precioUnitario      decimal
    DECLARE @iva      decimal
    DECLARE @arancel      uniqueidentifier
    DECLARE @link      varchar(300)
    DECLARE @tipoMoneda      int
    DECLARE @activo      bit
    DECLARE @idcategoria      int
    DECLARE @idtipoProducto      int
    DECLARE @idClasificacion      int
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','UNIQUEIDENTIFIER')
      ,@idProveedor = doc.col.value ('idProveedor[1]','UNIQUEIDENTIFIER')
      ,@numeroParte = doc.col.value ('numeroParte[1]','VARCHAR(50)')
      ,@idEmpleado = doc.col.value ('idEmpleado[1]','UNIQUEIDENTIFIER')
      ,@nombre = doc.col.value ('nombre[1]','VARCHAR(MAX)')
      ,@descripcion = doc.col.value ('descripcion[1]','VARCHAR(MAX)')
      ,@idMarca = doc.col.value ('idMarca[1]','INT')
      ,@grupoAsignado = doc.col.value ('grupoAsignado[1]','INT')
      ,@caracteristicas = doc.col.value ('caracteristicas[1]','VARCHAR(MAX)')
      ,@modelo = doc.col.value ('modelo[1]','VARCHAR(300)')
      ,@voltaje = doc.col.value ('voltaje[1]','VARCHAR(300)')
      ,@hertz = doc.col.value ('hertz[1]','VARCHAR(300)')
      ,@fechaIngreso = doc.col.value ('fechaIngreso[1]','DATETIME')
      ,@largo = doc.col.value ('largo[1]','DECIMAL')
      ,@ancho = doc.col.value ('ancho[1]','DECIMAL')
      ,@altura = doc.col.value ('altura[1]','DECIMAL')
      ,@umDimencion = doc.col.value ('umDimencion[1]','INT')
      ,@peso = doc.col.value ('peso[1]','DECIMAL')
      ,@umPeso = doc.col.value ('umPeso[1]','INT')
      ,@tipoRecogida = doc.col.value ('tipoRecogida[1]','VARCHAR(300)')
      ,@comision = doc.col.value ('comision[1]','VARCHAR(300)')
      ,@precioUnitario = doc.col.value ('precioUnitario[1]','DECIMAL')
      ,@iva = doc.col.value ('iva[1]','DECIMAL')
      ,@arancel = doc.col.value ('arancel[1]','UNIQUEIDENTIFIER')
      ,@link = doc.col.value ('link[1]','VARCHAR(300)')
      ,@tipoMoneda = doc.col.value ('tipoMoneda[1]','INT')
      ,@activo = doc.col.value ('activo[1]','BIT')
      ,@idcategoria = doc.col.value ('idcategoria[1]','INT')
      ,@idtipoProducto = doc.col.value ('idtipoProducto[1]','INT')
      ,@idClasificacion = doc.col.value ('idClasificacion[1]','INT')

       FROM
       @xml.nodes('/ROOT/ProductosVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  Productos WHERE   id=@id) ) 
       BEGIN
 
       	  INSERT INTO Productos(
           id,
           idProveedor,
           numeroParte,
           idEmpleado,
           nombre,
           descripcion,
           idMarca,
           grupoAsignado,
           imagen,
           caracteristicas,
           modelo,
           voltaje,
           hertz,
           fechaIngreso,
           largo,
           ancho,
           altura,
           umDimencion,
           peso,
           umPeso,
           tipoRecogida,
           comision,
           precioUnitario,
           iva,
           arancel,
           link,
           tipoMoneda,
           activo,
           idcategoria,
           idtipoProducto,
           idClasificacion)
           VALUES
            ( @id, @idProveedor, @numeroParte, @idEmpleado, @nombre, @descripcion, @idMarca, @grupoAsignado, null, @caracteristicas, @modelo, @voltaje, @hertz, @fechaIngreso, @largo, @ancho, @altura, @umDimencion, @peso, @umPeso, @tipoRecogida, @comision, @precioUnitario, @iva, @arancel, @link, @tipoMoneda, @activo, @idcategoria, @idtipoProducto, @idClasificacion) 
			SET @Resultado=SCOPE_IDENTITY()

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El Productos ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM Productos WHERE  id=@id  ))
           BEGIN

			    UPDATE Productos SET  

                   idProveedor = @idProveedor,
                   numeroParte = @numeroParte,
                   idEmpleado = @idEmpleado,
                   nombre = @nombre,
                   descripcion = @descripcion,
                   idMarca = @idMarca,
                   grupoAsignado = @grupoAsignado,
                   imagen = null,
                   caracteristicas = @caracteristicas,
                   modelo = @modelo,
                   voltaje = @voltaje,
                   hertz = @hertz,
                   fechaIngreso = @fechaIngreso,
                   largo = @largo,
                   ancho = @ancho,
                   altura = @altura,
                   umDimencion = @umDimencion,
                   peso = @peso,
                   umPeso = @umPeso,
                   tipoRecogida = @tipoRecogida,
                   comision = @comision,
                   precioUnitario = @precioUnitario,
                   iva = @iva,
                   arancel = @arancel,
                   link = @link,
                   tipoMoneda = @tipoMoneda,
                   activo = @activo,
                   idcategoria = @idcategoria,
                   idtipoProducto = @idtipoProducto,
                   idClasificacion = @idClasificacion 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El Productos NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE Productos WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END
return


GO

/****** Object:  StoredProcedure [dbo].[sp_crudPrioridades]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudPrioridades]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <31/05/2016>
-- Description: <Gestiona (actualiza) Prioridadess >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      tinyint
    DECLARE @descripcion      varchar(100)
    DECLARE @tiempoCumplimiento      int
    DECLARE @estado      bit
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','TINYINT')
      ,@descripcion = doc.col.value ('descripcion[1]','VARCHAR(100)')
      ,@tiempoCumplimiento = doc.col.value ('tiempoCumplimiento[1]','INT')
      ,@estado = doc.col.value ('estado[1]','BIT')

       FROM
       @xml.nodes('/ROOT/PrioridadesVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  Prioridades WHERE   id=@id) ) 
       BEGIN
 
       	  INSERT INTO Prioridades(
           id,
           descripcion,
           tiempoCumplimiento,
           estado)
           VALUES
            ( @id, @descripcion, @tiempoCumplimiento, @estado) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @id=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El Prioridades ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM Prioridades WHERE  id=@id  ))
           BEGIN

			    UPDATE Prioridades SET  

                   descripcion = @descripcion,
                   tiempoCumplimiento = @tiempoCumplimiento,
                   estado = @estado 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El Prioridades NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE Prioridades WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END



GO

/****** Object:  StoredProcedure [dbo].[sp_crudHistorial]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



create PROCEDURE [dbo].[sp_crudHistorial]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- Create date: <14/06/2016>
-- Description: <Gestiona (actualiza) Historials >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      varchar(40)
    DECLARE @fecha      datetime
    DECLARE @idEmpleado      varchar(40)
    DECLARE @descripcion      varchar(max)
    DECLARE @tipo      varchar(50)
    DECLARE @extension      varchar(50)
    DECLARE @idReferencia      varchar(40)
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','varchar(40)')
      ,@fecha = doc.col.value ('fecha[1]','DATETIME')
      ,@idEmpleado = doc.col.value ('idEmpleado[1]','varchar(40)')
      ,@descripcion = doc.col.value ('descripcion[1]','VARCHAR(max)')
      ,@tipo = doc.col.value ('tipo[1]','VARCHAR(50)')
      ,@extension = doc.col.value ('extension[1]','VARCHAR(50)')
      ,@idReferencia = doc.col.value ('idReferencia[1]','varchar(40)')

       FROM
       @xml.nodes('/ROOT/HistorialVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  Historial WHERE   id=@id) OR 1=1) 
       BEGIN
 
       	  INSERT INTO Historial(
           id,
           fecha,
           idEmpleado,
           descripcion,
           tipo,
           extension,
           documento,
           idReferencia)
           VALUES
            ( @id, @fecha, @idEmpleado, @descripcion, @tipo, @extension, null, @idReferencia) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @id=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El Historial ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM Historial WHERE  id=@id  ))
           BEGIN

			    UPDATE Historial SET  

                   fecha = @fecha,
                   idEmpleado = @idEmpleado,
                   descripcion = @descripcion,
                   tipo = @tipo,
                   extension = @extension,
                   documento = null,
                   idReferencia = @idReferencia 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El Historial NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE Historial WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END

RETURN

GO

/****** Object:  StoredProcedure [dbo].[sp_crudGruposTrabajo]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudGruposTrabajo]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <11/05/2016>
-- Description: <Gestiona (actualiza) GruposTrabajos >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      int
    DECLARE @nombre      varchar(300)
    DECLARE @sigla      varchar(50)
    DECLARE @descripcionActividad      varchar(500)
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','INT')
      ,@nombre = doc.col.value ('nombre[1]','VARCHAR(300)')
      ,@sigla = doc.col.value ('sigla[1]','VARCHAR(50)')
      ,@descripcionActividad = doc.col.value ('descripcionActividad[1]','VARCHAR(500)')

       FROM
       @xml.nodes('/ROOT/GruposTrabajoVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  GruposTrabajo WHERE   id=@id) ) 
       BEGIN
 
       	  INSERT INTO GruposTrabajo(
           id,
           nombre,
           sigla,
           descripcionActividad)
           VALUES
            ( @id, @nombre, @sigla, @descripcionActividad) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @id=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El GruposTrabajo ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM GruposTrabajo WHERE  id=@id  ))
           BEGIN

			    UPDATE GruposTrabajo SET  

                   nombre = @nombre,
                   sigla = @sigla,
                   descripcionActividad = @descripcionActividad 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El GruposTrabajo NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE GruposTrabajo WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END



GO

/****** Object:  StoredProcedure [dbo].[sp_crudGruposProceso]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



create PROCEDURE [dbo].[sp_crudGruposProceso]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- Create date: <27/05/2016>
-- Description: <Gestiona (actualiza) GruposProcesos >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @idProceso      varchar(40)
    DECLARE @idGrupo      int
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @idProceso = doc.col.value ('idProceso[1]','varchar(40)')
      ,@idGrupo = doc.col.value ('idGrupo[1]','INT')

       FROM
       @xml.nodes('/ROOT/GruposProcesoVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  GruposProceso WHERE   idProceso=@idProceso)  OR 1=1) 
       BEGIN
 
       	  INSERT INTO GruposProceso(
           idProceso,
           idGrupo)
           VALUES
            ( @idProceso, @idGrupo) 
			SET @Resultado=SCOPE_IDENTITY()

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El GruposProceso ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM GruposProceso WHERE  idProceso=@idProceso  ) OR 1=1)
           BEGIN

			    UPDATE GruposProceso SET  

                   idGrupo = @idGrupo 
					WHERE  idProceso=@idProceso

        END
			ELSE 
        BEGIN

					RAISERROR ('El GruposProceso NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE GruposProceso WHERE idProceso=@idProceso

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END


GO

/****** Object:  StoredProcedure [dbo].[sp_crudEncuentasTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudEncuentasTicket]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <10/06/2016>
-- Description: <Gestiona (actualiza) EncuentasTickets >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      varchar(40)
    DECLARE @idTicket      varchar(40)
    DECLARE @idPregunta      varchar(40)
    DECLARE @respuesta      int
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','varchar(40)')
      ,@idTicket = doc.col.value ('idTicket[1]','varchar(40)')
      ,@idPregunta = doc.col.value ('idPregunta[1]','varchar(40)')
      ,@respuesta = doc.col.value ('respuesta[1]','INT')

       FROM
       @xml.nodes('/ROOT/EncuentasTicketVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  EncuentasTicket WHERE   idTicket=@idTicket) or 1=1) 
       BEGIN
 
       	  INSERT INTO EncuentasTicket(
           id,
           idTicket,
           idPregunta,
           respuesta)
           VALUES
            ( NEWID(), @idTicket, @idPregunta, @respuesta) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @idTicket=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El EncuentasTicket ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM EncuentasTicket WHERE  idTicket=@idTicket  ) OR 1=1)
           BEGIN

			    UPDATE EncuentasTicket SET  
                    idTicket=@idTicket,
                   idPregunta = @idPregunta,
                   respuesta = @respuesta 
					WHERE id = @id

        END
			ELSE 
        BEGIN

					RAISERROR ('El EncuentasTicket NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE EncuentasTicket WHERE idTicket=@idTicket

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END



GO

/****** Object:  StoredProcedure [dbo].[sp_crudEmpresas]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudEmpresas]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        varchar(100) output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <18/04/2016>
-- Description: <Gestiona (actualiza) Empresass >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      uniqueidentifier
    DECLARE @tipoEmpresa      char(3)
    DECLARE @identificacion      varchar(50)
    DECLARE @dv      char(1)
    DECLARE @razonSocial      varchar(500)
    DECLARE @antiguedad      int
    DECLARE @telefono      varchar(500)
    DECLARE @fax      varchar(500)
    DECLARE @direccion      varchar(800)
    DECLARE @mail      varchar(500)
    DECLARE @url      varchar(500)
    DECLARE @ciudad      varchar(300)
    DECLARE @departamento      varchar(300)
    DECLARE @pais      varchar(300)
    DECLARE @aniversario      datetime
    DECLARE @tieneAniversario      bit
    DECLARE @fechaIngreso      datetime
    DECLARE @idActividadEconomica      int
    DECLARE @movil      varchar(500)
    DECLARE @observaciones      varchar(MAX)
    DECLARE @seguimientoHistorico      varchar(MAX)
    DECLARE @estado      tinyint
    DECLARE @idAsesor      uniqueidentifier
    DECLARE @representanteLegal      varchar(100)
    DECLARE @idTipoMonedaImportacion      int
    DECLARE @tipoNacionalidad      bit
    DECLARE @idEmpleadoModif      uniqueidentifier
    DECLARE @codigoInterno      int
    DECLARE @idResponsableFacturacion      uniqueidentifier
    DECLARE @idResponsableServicio      uniqueidentifier
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','UNIQUEIDENTIFIER')
      ,@tipoEmpresa = doc.col.value ('tipoEmpresa[1]','CHAR(3)')
      ,@identificacion = doc.col.value ('identificacion[1]','VARCHAR(50)')
      ,@dv = doc.col.value ('dv[1]','CHAR(1)')
      ,@razonSocial = doc.col.value ('razonSocial[1]','VARCHAR(500)')
      ,@antiguedad = doc.col.value ('antiguedad[1]','INT')
      ,@telefono = doc.col.value ('telefono[1]','VARCHAR(500)')
      ,@fax = doc.col.value ('fax[1]','VARCHAR(500)')
      ,@direccion = doc.col.value ('direccion[1]','VARCHAR(800)')
      ,@mail = doc.col.value ('mail[1]','VARCHAR(500)')
      ,@url = doc.col.value ('url[1]','VARCHAR(500)')
      ,@ciudad = doc.col.value ('ciudad[1]','VARCHAR(300)')
      ,@departamento = doc.col.value ('departamento[1]','VARCHAR(300)')
      ,@pais = doc.col.value ('pais[1]','VARCHAR(300)')
      ,@aniversario = doc.col.value ('aniversario[1]','DATETIME')
      ,@tieneAniversario = doc.col.value ('tieneAniversario[1]','BIT')
      ,@fechaIngreso = doc.col.value ('fechaIngreso[1]','DATETIME')
      ,@idActividadEconomica = doc.col.value ('idActividadEconomica[1]','INT')
      ,@movil = doc.col.value ('movil[1]','VARCHAR(500)')
      ,@observaciones = doc.col.value ('observaciones[1]','VARCHAR(MAX)')
      ,@seguimientoHistorico = doc.col.value ('seguimientoHistorico[1]','VARCHAR(MAX)')
      ,@estado = doc.col.value ('estado[1]','TINYINT')
      ,@idAsesor = doc.col.value ('idAsesor[1]','UNIQUEIDENTIFIER')
      ,@representanteLegal = doc.col.value ('representanteLegal[1]','VARCHAR(100)')
      ,@idTipoMonedaImportacion = doc.col.value ('idTipoMonedaImportacion[1]','INT')
      ,@tipoNacionalidad = doc.col.value ('tipoNacionalidad[1]','BIT')
      ,@idEmpleadoModif = doc.col.value ('idEmpleadoModif[1]','UNIQUEIDENTIFIER')
      ,@codigoInterno = doc.col.value ('codigoInterno[1]','INT')
      ,@idResponsableFacturacion = doc.col.value ('idResponsableFacturacion[1]','UNIQUEIDENTIFIER')
      ,@idResponsableServicio = doc.col.value ('idResponsableServicio[1]','UNIQUEIDENTIFIER')

       FROM
       @xml.nodes('/ROOT/EmpresasVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  Empresas WHERE   id=@id) ) 
       BEGIN
 
       	  INSERT INTO Empresas(
           id,
           tipoEmpresa,
           identificacion,
           dv,
           razonSocial,
           antiguedad,
           telefono,
           fax,
           direccion,
           mail,
           url,
           ciudad,
           departamento,
           pais,
           aniversario,
           tieneAniversario,
           fechaIngreso,
           idActividadEconomica,
           movil,
           observaciones,
           seguimientoHistorico,
           estado,
           idAsesor,
           representanteLegal,
           idTipoMonedaImportacion,
           tipoNacionalidad,
           idEmpleadoModif,
           codigoInterno,
           idResponsableFacturacion,
           idResponsableServicio)
           VALUES
            ( @id, @tipoEmpresa, @identificacion, @dv, @razonSocial, @antiguedad, @telefono, @fax, @direccion, @mail, @url, @ciudad, @departamento, @pais, @aniversario, @tieneAniversario, @fechaIngreso, @idActividadEconomica, @movil, @observaciones, @seguimientoHistorico, @estado, @idAsesor, @representanteLegal, @idTipoMonedaImportacion, @tipoNacionalidad, @idEmpleadoModif, @codigoInterno, @idResponsableFacturacion, @idResponsableServicio) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @id=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El Empresas ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM Empresas WHERE  id=@id  ))
           BEGIN

			    UPDATE Empresas SET  

                   tipoEmpresa = @tipoEmpresa,
                   identificacion = @identificacion,
                   dv = @dv,
                   razonSocial = @razonSocial,
                   antiguedad = @antiguedad,
                   telefono = @telefono,
                   fax = @fax,
                   direccion = @direccion,
                   mail = @mail,
                   url = @url,
                   ciudad = @ciudad,
                   departamento = @departamento,
                   pais = @pais,
                   aniversario = @aniversario,
                   tieneAniversario = @tieneAniversario,
                   fechaIngreso = @fechaIngreso,
                   idActividadEconomica = @idActividadEconomica,
                   movil = @movil,
                   observaciones = @observaciones,
                   seguimientoHistorico = @seguimientoHistorico,
                   estado = @estado,
                   idAsesor = @idAsesor,
                   representanteLegal = @representanteLegal,
                   idTipoMonedaImportacion = @idTipoMonedaImportacion,
                   tipoNacionalidad = @tipoNacionalidad,
                   idEmpleadoModif = @idEmpleadoModif,
                   idResponsableFacturacion = @idResponsableFacturacion,
                   idResponsableServicio = @idResponsableServicio 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El Empresas NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE Empresas WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH

	RETURN
END



GO

/****** Object:  StoredProcedure [dbo].[sp_crudDispositivo]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudDispositivo]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <18/05/2016>
-- Description: <Gestiona (actualiza) Dispositivos >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @id      uniqueidentifier
    DECLARE @descripcion      varchar(500)
    DECLARE @idCliente      uniqueidentifier
    DECLARE @estado      bit
    DECLARE @idTipo      uniqueidentifier
    DECLARE @idMarca      int
    DECLARE @serial      varchar(150)
    DECLARE @numeroParte      varchar(150)
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @id = doc.col.value ('id[1]','UNIQUEIDENTIFIER')
      ,@descripcion = doc.col.value ('descripcion[1]','VARCHAR(500)')
      ,@idCliente = doc.col.value ('idCliente[1]','UNIQUEIDENTIFIER')
      ,@estado = doc.col.value ('estado[1]','BIT')
      ,@idTipo = doc.col.value ('idTipo[1]','UNIQUEIDENTIFIER')
      ,@idMarca = doc.col.value ('idMarca[1]','INT')
      ,@serial = doc.col.value ('serial[1]','VARCHAR(150)')
      ,@numeroParte = doc.col.value ('numeroParte[1]','VARCHAR(150)')

       FROM
       @xml.nodes('/ROOT/DispositivoVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  Dispositivo WHERE   id=@id) ) 
       BEGIN
 
       	  INSERT INTO Dispositivo(
           id,
           descripcion,
           idCliente,
           estado,
           idTipo,
           idMarca,
           serial,
           numeroParte)
           VALUES
            ( @id, @descripcion, @idCliente, @estado, @idTipo, @idMarca, @serial, @numeroParte) 
			SET @Resultado=SCOPE_IDENTITY()

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El Dispositivo ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM Dispositivo WHERE  id=@id  ))
           BEGIN

			    UPDATE Dispositivo SET  

                   descripcion = @descripcion,
                   idCliente = @idCliente,
                   estado = @estado,
                   idTipo = @idTipo,
                   idMarca = @idMarca,
                   serial = @serial,
                   numeroParte = @numeroParte 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El Dispositivo NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE Dispositivo WHERE id=@id

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END
return


GO

/****** Object:  StoredProcedure [dbo].[sp_crudDetalletTicket]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[sp_crudDetalletTicket]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        varchar(120) output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- Create date: <19/05/2016>
-- Description: <Gestiona (actualiza) DetalletTickets >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	declare @id varchar(40)
    DECLARE @idTicket      varchar(40)
    DECLARE @idServicio      varchar(40)
    DECLARE @idDispositivo      varchar(40)
    DECLARE @idSuscripcion      varchar(40)
    DECLARE @idTipo      int
    DECLARE @descripcion      varchar(MAX)
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
	 @id = doc.col.value ('id[1]','varchar(40)'),
	 @idTicket = doc.col.value ('idTicket[1]','varchar(40)'),
      @idServicio = doc.col.value ('idServicio[1]','varchar(40)')
      ,@idDispositivo = doc.col.value ('idDispositivo[1]','varchar(40)')
      ,@idSuscripcion = doc.col.value ('idSuscripcion[1]','varchar(40)')
      ,@idTipo = doc.col.value ('idTipo[1]','INT')
      ,@descripcion = doc.col.value ('descripcion[1]','VARCHAR(MAX)')

       FROM
       @xml.nodes('/ROOT/DetalletTicketVO') doc(col)

	   --declare @idtemp varchar(40)
	   select @idTicket = convert(varchar(40),Id) from Ticket where CodigoInterno = convert(int,@idTicket)

	 --  if(@idtemp IS NULL) select @idTicket = convert(varchar(40),NEWID())
		--else select @idTicket =  @idtemp

       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(1 = 1) 
       BEGIN
		
       	  INSERT INTO DetalletTicket(
           id,
           idTicket,
           idServicio,
           idDispositivo,
           idSuscripcion,
           idTipo,
           descripcion)
           VALUES
            (@id ,@idTicket, @idServicio, @idDispositivo, @idSuscripcion, @idTipo, @descripcion) 
			SET @Resultado=SCOPE_IDENTITY()
		--	select * from DetalletTicket order by codigointerno desc
			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El DetalletTicket ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM DetalletTicket WHERE 1=1  ))
           BEGIN

			    UPDATE DetalletTicket SET  

                   idTicket = @idTicket,
                   idServicio = @idServicio,
                   idDispositivo = @idDispositivo,
                   idSuscripcion = @idSuscripcion,
                   idTipo = @idTipo,
                   descripcion = @descripcion 
					WHERE  id=@id

        END
			ELSE 
        BEGIN

					RAISERROR ('El DetalletTicket NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE DetalletTicket WHERE Id = @Id
			

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END
RETURN

GO

/****** Object:  StoredProcedure [dbo].[sp_crudAgenda]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[sp_crudAgenda]
(
	@accion		        int,    --> 1:INSERTAR , 2:ELIMINAR, 3 ELIMINAR
	@xmlObj		        varchar(MAX),
	@usuarioOperacion	varchar(120),
	@Resultado	        int output
)
AS
-- =============================================
-- Author: ALVARO ORTIZ CASTELLANOS
-- alter date: <13/06/2016>
-- Description: <Gestiona (actualiza) Agendas >
-- ============================================= 

BEGIN
	--ESTABLECER EL NOMBRE DEL PROCEDIMIENTO
	DECLARE @nombre_procedimiento varchar(256)=OBJECT_NAME(@@PROCID)		
	DECLARE @xml XML;
	
    DECLARE @idRow      varchar(40)
    DECLARE @asunto      varchar(800)
    DECLARE @fechaElaboracion      datetime
    DECLARE @fechainicio      datetime
    DECLARE @fechaFin      datetime
    DECLARE @descripcion      varchar(max)
    DECLARE @seguimiento      varchar(max)
    DECLARE @idReferencia      varchar(40)
    DECLARE @responsable      varchar(40)
    DECLARE @idViacontacto      int
    DECLARE @idAutor      varchar(40)
    DECLARE @estado      bit
    DECLARE @fechaCierre      datetime
    DECLARE @recordatorio      bit
    DECLARE @programada      bit
    DECLARE @orden      int
	 
	--VARIABLES DE ERROR
	DECLARE @ErrorMessage nvarchar(256)
	DECLARE @ErrorNumber int	
	DECLARE @ErrorState int		
	DECLARE @ErrorSeverity int	
	 
 
   --EJEMPLO PARA DISPARAR EXCEPCION 
   --RAISERROR ('Error VALIDANDO NOMBRE DE FORMACION.', -- Message text. 
   --              16, -- Severity. 
   --              1 -- State. 
   --              ); 
 
   -->  INICIAR TRANSACCIÓN 
	BEGIN TRAN

	BEGIN TRY

	SET @xml = @xmlObj;

	SELECT 
      @idRow = doc.col.value ('idRow[1]','varchar(40)')
      ,@asunto = doc.col.value ('asunto[1]','VARCHAR(800)')
      ,@fechaElaboracion = doc.col.value ('fechaElaboracion[1]','DATETIME')
      ,@fechainicio = doc.col.value ('fechainicio[1]','DATETIME')
      ,@fechaFin = doc.col.value ('fechaFin[1]','DATETIME')
      ,@descripcion = doc.col.value ('descripcion[1]','VARCHAR(max)')
      ,@seguimiento = doc.col.value ('seguimiento[1]','VARCHAR(max)')
      ,@idReferencia = doc.col.value ('idReferencia[1]','varchar(40)')
      ,@responsable = doc.col.value ('responsable[1]','varchar(40)')
      ,@idViacontacto = doc.col.value ('idViacontacto[1]','INT')
      ,@idAutor = doc.col.value ('idAutor[1]','varchar(40)')
      ,@estado = doc.col.value ('estado[1]','BIT')
      ,@fechaCierre = doc.col.value ('fechaCierre[1]','DATETIME')
      ,@recordatorio = doc.col.value ('recordatorio[1]','BIT')
      ,@programada = doc.col.value ('programada[1]','BIT')
      ,@orden = doc.col.value ('orden[1]','INT')

       FROM
       @xml.nodes('/ROOT/AgendaVO') doc(col)
    
       IF(@accion=1)--INSERTAR
       BEGIN
    
       	IF(NOT EXISTS(SELECT 1 FROM  Agenda WHERE   idRow=@idRow) OR 1=1) 
       BEGIN
 
       	  INSERT INTO Agenda(
           idRow,
           asunto,
           fechaElaboracion,
           fechainicio,
           fechaFin,
           descripcion,
           seguimiento,
           idReferencia,
           responsable,
           idViacontacto,
           idAutor,
           estado,
           fechaCierre,
           recordatorio,
           programada,
           orden)
           VALUES
            ( @idRow, @asunto, @fechaElaboracion, @fechainicio, @fechaFin, @descripcion, @seguimiento, @idReferencia, @responsable, @idViacontacto, @idAutor, @estado, @fechaCierre, @recordatorio, @programada, @orden) 
			SET @Resultado=SCOPE_IDENTITY()
			SET @idRow=@Resultado;

			--INSERTAR EN UNA TABLA DE VERIFICACION
       END
	ELSE 
       BEGIN

			RAISERROR ('El Agenda ya se encuentra registrado', -- Message text.
				  16, -- Severity.
				  1 -- State.
				  );
       END
END

       IF (@accion=2) -->ACTUALIZAR
       BEGIN

		    IF(EXISTS(SELECT 1 FROM Agenda WHERE  idRow=@idRow  ))
           BEGIN

			    UPDATE Agenda SET  

                   asunto = @asunto,
                   fechaElaboracion = @fechaElaboracion,
                   fechainicio = @fechainicio,
                   fechaFin = @fechaFin,
                   descripcion = @descripcion,
                   seguimiento = @seguimiento,
                   idReferencia = @idReferencia,
                   responsable = @responsable,
                   idViacontacto = @idViacontacto,
                   idAutor = @idAutor,
                   estado = @estado,
                   fechaCierre = @fechaCierre,
                   recordatorio = @recordatorio,
                   programada = @programada,
                   orden = @orden 
					WHERE  idRow=@idRow

        END
			ELSE 
        BEGIN

					RAISERROR ('El Agenda NO se encuentra registrado', -- Message text.
						  16, -- Severity.
						  1 -- State.
						  );
        END
        END

		IF (@accion=3)--ELIMINAR
        BEGIN

			DELETE Agenda WHERE idRow=@idRow

        End

		--FINALIZAR LA TRANSACCION
		IF ISNULL(@ErrorNumber,0)=0 AND @@TRANCOUNT>0 BEGIN
			SET @ErrorNumber=0
           COMMIT
		END ELSE IF ISNULL(@ErrorNumber,0)!=0 BEGIN 	
           ROLLBACK
        END
	END TRY

	BEGIN CATCH
		SET @ErrorNumber=ERROR_NUMBER()
		SET @ErrorState=ERROR_STATE()
		SET @ErrorMessage=ERROR_MESSAGE()
		SET @ErrorSeverity = ERROR_SEVERITY()

		--devolver la transacción

		IF @@TRANCOUNT >0 BEGIN
            ROLLBACK
            END

		 RAISERROR (@ErrorMessage, -- Message text.
               @ErrorSeverity, -- Severity.
               @ErrorState -- State.
               )

		--EXEC PARAMETRIZACION.PROCEDIMIENTO_GRABAR_LOG_ERRORES @codigoerror, @descripcionError, @nombre_procedimiento, @usuario_operacion
	END CATCH
END
RETURN


GO

/****** Object:  StoredProcedure [dbo].[ResetearTiempos]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE PROC [dbo].[ResetearTiempos]
(
@Idparam uniqueidentifier = null
)
As
SET XACT_ABORT OFF
--DECLARACIONES
DECLARE @Id uniqueidentifier;
DECLARE @IdPlan uniqueidentifier;
declare @IdTipoSoporte uniqueidentifier;
declare @Horas int


DECLARE cusor CURSOR FOR  
SELECT Id, IdPlan
FROM     Suscripcion
where Id = @Idparam OR @Idparam is null

OPEN cusor;
FETCH NEXT FROM cusor 
INTO @Id, @IdPlan;

WHILE @@FETCH_STATUS = 0
   BEGIN     


      DECLARE cusor2 CURSOR FOR  
SELECT IdTipoSoporte, Horas
FROM     TiemposContrato
where IdContrato = @IdPlan

OPEN cusor2;
FETCH NEXT FROM cusor2 
INTO @IdTipoSoporte, @Horas;

WHILE @@FETCH_STATUS = 0
   BEGIN     
   
   begin try
   INSERT INTO [dbo].[TiempoSuscripcion]
           ([IdSuscripcion]
           ,[IdPlan]
           ,[IdTipoSoporte]
           ,[TiempoRestante])
     VALUES
           (@Id, @IdPlan, @IdTipoSoporte, @Horas*3600)
   end try
   
   begin catch
  
   UPDATE [dbo].[TiempoSuscripcion]
   SET TiempoRestante = @Horas*3600
 WHERE IdPlan = @IdPlan
      AND IdSuscripcion = @Id
      AND IdTipoSoporte = @IdTipoSoporte
   end    catch  

       
      FETCH NEXT FROM cusor2       
    INTO @IdTipoSoporte, @Horas;      
   END;
CLOSE cusor2;
DEALLOCATE cusor2;


       
      FETCH NEXT FROM cusor       
    INTO @Id, @IdPlan;      
   END;
CLOSE cusor;
DEALLOCATE cusor;


GO

/****** Object:  StoredProcedure [dbo].[AvanzarTecnico]    Script Date: 11/11/2016 5:32:28 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE PROC [dbo].[AvanzarTecnico]
(
	@Resultado	        varchar(40) output
)
As
--DECLARACIONES
DECLARE @puestoActual int;
DECLARE @puestoMax int;
DECLARE @puestoMin int;

select @puestoActual = TurnoEmpleado from DatosGenerales

if(@puestoActual IS NULL)
 set @puestoActual = (select top 1 Orden from Empleados where Orden <> -1 order by Orden)

  set @puestoMax = (select top 1 Orden from Empleados where Orden <> -1 order by Orden DESC)
   set @puestoMin = (select top 1 Orden from Empleados where Orden <> -1 order by Orden)

   --SE SELECCIONA AL EMPLEADO ACTUAL PARA DEVOLVER
   set @Resultado = (@puestoActual)

   --SE ASCIENDE EN UNO EL CONTADOR
   if(@puestoMax = @puestoActual)
   update DatosGenerales set TurnoEmpleado = (@puestoMin)
   else  update DatosGenerales set TurnoEmpleado = (@puestoActual+1)

   return @Resultado



GO


USE [MVCommon]
GO

/****** Object:  Trigger [dbo].[trig_Insert_Suscripcion]    Script Date: 11/11/2016 5:37:11 p. m. ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE TRIGGER [dbo].[trig_Insert_Suscripcion]
ON [dbo].[Suscripcion]
FOR INSERT, UPDATE
AS
Begin
  exec ResetearTiempos  (select Id from Inserted)
End

update Suscripcion set Descripcion = Descripcion where Codigo = 1

GO





