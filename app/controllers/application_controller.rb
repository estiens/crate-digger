class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception
  
  private 

  def render_error_response(error)
    render json: error, serializer: ApiExceptionsSerializer, status: 422
  end
end
